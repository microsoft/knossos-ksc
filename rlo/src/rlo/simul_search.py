# fmt: off
# mypy: ignore-errors
import numpy
from typing import Callable, Generator, Iterable, Optional, Tuple
import os

from rlo import analytics
from rlo.dataset import PolicyNetDataset
from rlo.dataset_refiner import DatasetRefiner, DatasetRefinerFactory
from rlo.distillator import Distillator
from rlo.expr_shuffler import ExpressionShuffler
from rlo.expression_util import NamedExprWithEnv
from rlo.factory import ModelStateFactory
from rlo.tf_model import Weights
from rlo.compute_values import compute_values
from rlo.reporting.azureml import AzuremlReporter
from rlo.search_ops import AbstractSearcher
from rlo.training_run_state import TrainingRunState
from rlo import utils
from rlo.worker import AsTrainModel, AsEvalModel, Accumulator, RunMultiple, RunOnGPU


class SimultaneousSearchCurriculum:
    """ Mutable curriculum that runs on headnode, scheduling eval/search/training/distillation jobs on workers.
        Searches all expressions, in parallel, using the same weights; then trains on the merged dataset. """
    def __init__(self,
                 train_exprs_per_generation: ExpressionShuffler,
                 train_searcher: AbstractSearcher,
                 eval_expressions: Iterable[NamedExprWithEnv],
                 eval_searcher: AbstractSearcher,
                 distillator: Distillator,
                 dataset_refiner_factory: DatasetRefinerFactory,
                 rng_for_gen_fn: Callable[[int], numpy.random.Generator],
                 model_save_dir: str,
                 reporter: AzuremlReporter,
                 model_state_factory: ModelStateFactory,
                 num_generations: Optional[int] = None,
                 total_train_time: Optional[int] = None,
                 save_all_models: bool = False,
                 repetition: int = 0,
    ):
        self._train_exprs_per_generation = train_exprs_per_generation
        self._num_generations = num_generations
        self._total_train_time = total_train_time
        if self._num_generations is None and self._total_train_time is None:
            raise ValueError("Must specify at least one of num_generations and total_train_time")
        self._train_searcher = train_searcher
        self._eval_searcher = eval_searcher
        self._eval_expressions = eval_expressions
        self._distillator = distillator
        self._dataset_refiner_factory = dataset_refiner_factory
        self._rng_for_gen_fn = rng_for_gen_fn
        self._save_all_models = save_all_models
        self._model_save_dir = model_save_dir
        self._repetition = repetition
        self._reporter = reporter
        self._model_state_factory = model_state_factory

    def initial_state(self) -> TrainingRunState:
        return TrainingRunState(self._train_searcher.initial_state(), self._dataset_refiner_factory(), 0, 0.0)

    def try_load_previous_run(self, allow_loading_just_model=True) -> Optional[Tuple[Weights, TrainingRunState]]:
         # Look for saved state from previous invocation of same repetition and run_id
        saved_models = utils.get_saved_model_files(self._model_save_dir)
        if len(saved_models) == 0:
            return None
        generation, model_file = saved_models[-1]
        weights = self._model_state_factory.from_path(model_file)
        saved_state = TrainingRunState.try_load(self._model_save_dir, generation)
        if saved_state is None:
            if not allow_loading_just_model:
                return None
            saved_state = self.initial_state()
        analytics.event("restored_model", model_file=str(model_file), generation=generation)
        return weights, saved_state

    def request_initial(self, weights_seed: int):
        """Tries to resume from any weights+state saved by a previous invocation of the
        same repetition and run_id. Starts afresh if no saved weights are found.
        """
        saved = self.try_load_previous_run()
        if saved is None:
            weights = yield RunOnGPU(f"seedweights_r{self._repetition}",
                                     weights_seed, lambda model_wrapper: model_wrapper.get_weights(),
                                     urgency=(-1, 0.5)) # Lower than generation-1 search but higher than evaluation
            yield from self.request_eval(weights, 0, 0.0, utils.rng(self._rng_for_gen_fn(0)), analytics.Scope(generation=0))
            state = self.initial_state()
        else:
            weights, state = saved
        yield from self.request_training(weights, state)

    def request_eval(self, weights: Weights, generation: int, total_train_time: float, eval_rng, scope):
        # The urgency is to be lowest among the other operations using the same weights
        # - i.e. the search/distill ops in the next generation, see request_training_below.
        eval_urgency = (-(generation+1), 0)
        name_suffix = f"{generation}r{self._repetition}"
        scope = analytics.Scope(scope, total_train_time=total_train_time)
        # TODO #19452 we don't need the 'RunMultiple' here
        search_summaries = yield RunMultiple([
            RunOnGPU(f"eval_{expr.name}_{name_suffix}", weights,
                     AsEvalModel().then(self._eval_searcher.eval, utils.seed(eval_rng), expr, untrained_model=(generation==0)),
                     urgency=eval_urgency,
                     scope=scope,
                     wait_for_result=True)
                for expr in self._eval_expressions
        ])
        self._reporter.log_raw_train_test_metrics(generation, search_summaries)
        yield RunMultiple([
            # Get value estimates from the model for each expression at each time-left.
            # We'll log these out for every generation.
            RunOnGPU(f"stateval_{name_suffix}", weights,
                AsEvalModel().then(compute_values, self._eval_expressions),
                urgency=eval_urgency, scope=scope, wait_for_result=False)
        ])

    def _finished(self, run_state: TrainingRunState):
        return ((self._num_generations is not None and run_state.generation >= self._num_generations)
            or (self._total_train_time is not None and run_state.total_train_time >= self._total_train_time))

    def request_training(self, weights: Weights, run_state: TrainingRunState) -> Generator:
        """Generator function for WorkRequests to train from the specified state to the end of the run."""
        while not self._finished(run_state):
            # Begin the new generation
            generation = run_state.generation + 1
            total_train_time = Accumulator(run_state.total_train_time)
            search_state = run_state.searcher_state
            dataset_refiner = run_state.dataset_refiner
            del run_state
            # These are priorities within this generation - i.e., all for the same set of input weights.
            # The best priority assignment is still uncertain but currently we prioritize earlier generations over later ones
            # (so we can finish using old weights ASAP and keep the window of live objects small).
            # We prioritize distillation highest because it's longest-running (and a repetition doing distillation, will
            # have little other work it can do at the same time - only evaluation which is usually short);
            # note distillation will never compete with search *of the same repetition*, and the task scheduler
            # is FIFO so will execute all search jobs of one repetition before starting the next (as desired).
            search_urgency = (-generation, 1)
            distill_urgency = (-generation, 2)

            # We'll use this rng for everything in this call
            generation_rng = self._rng_for_gen_fn(generation)
            eval_rng = utils.rng(generation_rng) # Draw first (but use later)
            # Get a set of starting expressions for this generation
            starting_expr_batch = self._train_exprs_per_generation[generation]
            generation_scope = analytics.Scope(generation=generation)
            name_suffix = f"{generation}r{self._repetition}"

            # Search on all expressions in parallel.
            expr_results = yield RunMultiple([
                RunOnGPU(f"search_{expr_name}_{name_suffix}", weights,
                        AsTrainModel().then(self._train_searcher.training_search, utils.seed(generation_rng), expr, **search_state.get_state_for_expr(expr)),
                        urgency=search_urgency,
                        time_acc=total_train_time,
                        scope=analytics.Scope(generation_scope, expr=expr_name, expr_cost=expr.cost()))
                for expr_name, expr in starting_expr_batch
            ])
            # All exprs have returned. Train on the merge.
            merged_dataset = self._train_searcher.get_empty_dataset()
            for dataset, (expr_name, expr) in zip(expr_results, starting_expr_batch):
                with analytics.Scope(generation_scope, expr=expr_name, expr_cost=expr.cost()):
                    search_state.update_state_for_expr(expr, dataset)
                    if isinstance(dataset, PolicyNetDataset):
                        assert type(dataset_refiner) is DatasetRefiner, "Only no-op refiner (DatasetRefiner) is supported for PolicyNetDataset"
                    dataset = dataset_refiner(expr, dataset)
                    # Refiner may return a None dataset
                    if dataset is not None:
                        merged_dataset.merge_dataset(dataset)

            if merged_dataset.has_points():
                weights = yield from weights.request_distill(
                    f"{generation}r{self._repetition}",
                    self._distillator,
                    merged_dataset,
                    utils.seed(generation_rng),
                    distill_urgency,
                    total_train_time,
                    generation_scope)
                # Only if we have new weights, do we evaluate
                yield from self.request_eval(weights, generation, total_train_time.value, eval_rng, generation_scope)
            # else - training skipped, as we didn't get any data to train on (e.g. no expressions succeeded).
            # Let's hope we have easier expressions to search in the next generation!
            gen_end_time = total_train_time.value
            # Kill off the old Accumulator as a check that nothing whose time should count is still running
            total_train_time._value = object()
            run_state = TrainingRunState(search_state, dataset_refiner, generation, gen_end_time)
            if self._save_all_models or self._finished(run_state):
                weights.save(os.path.join(utils.get_model_save_path(self._model_save_dir, str(generation)), "model_best.npz"))
                run_state.save(self._model_save_dir)
