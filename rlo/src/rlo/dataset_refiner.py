from typing import Callable, Dict, List, Optional

from rlo import analytics
from rlo.dataset import Dataset, StateValueDataset
from rlo.expression_util import ExprWithEnv
from rlo.search_ops import min_cost_achieved


class DatasetRefiner:
    """No-op base class for all dataset refiners."""

    def __call__(
        self, exprenv: ExprWithEnv, dataset: Optional[Dataset]
    ) -> Optional[Dataset]:
        return dataset


class ReuseRefiner(DatasetRefiner):
    """Refiner which reuses the previous dataset if the current one is strictly worse."""

    def __init__(self, inner_refiner: DatasetRefiner):
        self._inner_refiner = inner_refiner
        self._last_good_dataset: Dict = {}

    def __call__(
        self, exprenv: ExprWithEnv, dataset: Optional[Dataset]
    ) -> Optional[Dataset]:
        # Apply the inner refiner first.
        dataset = self._inner_refiner(exprenv, dataset)

        if dataset is None:
            # If a previous refiner already decided we should skip training (or
            # `dataset` was `None` from the very beginning), then return.
            return None
        min_cost = min_cost_achieved(exprenv, dataset)

        prev_res = self._last_good_dataset.get(exprenv)

        if prev_res is None:
            if min_cost >= exprenv.cost():
                analytics.event("skip_no_successes")
                return None
        else:
            prev_min_cost = min_cost_achieved(exprenv, prev_res)
            if min_cost > prev_min_cost:
                analytics.event(
                    "reuse_dataset", min_cost=min_cost, prev_min_cost=prev_min_cost
                )
                return prev_res

        self._last_good_dataset[exprenv] = dataset

        return dataset


class BestAcrossGenerationsRefiner(DatasetRefiner):
    """Refiner which keeps a cache of best value seen for every input, and updates the datasets to match that."""

    def __init__(self, inner_refiner: DatasetRefiner):
        self._inner_refiner = inner_refiner
        self._best_value_cache = StateValueDataset()

    def _update_cache(self, dataset: StateValueDataset) -> None:
        num_expressions_in_cache_before = self._best_value_cache.num_expressions()

        self._best_value_cache.merge_dataset(dataset)

        analytics.event(
            "update_cache_across_generations",
            num_expressions_in_cache_before=num_expressions_in_cache_before,
            num_expressions_in_cache_after=self._best_value_cache.num_expressions(),
            num_expressions_in_dataset=dataset.num_expressions(),
        )

    def _merge_with_cache(self, dataset: StateValueDataset) -> None:
        expressions_changed = 0

        for expr in dataset.exprenvs:
            if dataset.copy_sequence_from(self._best_value_cache, expr):
                expressions_changed += 1

        analytics.event(
            "merge_with_cache_across_generations",
            num_expressions_in_dataset=dataset.num_expressions(),
            num_expressions_changed=expressions_changed,
        )

    def __call__(
        self, exprenv: ExprWithEnv, dataset: Optional[Dataset]
    ) -> Optional[StateValueDataset]:

        # Update the cache first...
        if isinstance(dataset, StateValueDataset):
            self._update_cache(dataset)

        # ...then apply the inner refiner...
        dataset = self._inner_refiner(exprenv, dataset)

        if dataset is None:
            # If a previous refiner decided we should skip training then return.
            return None

        if not isinstance(dataset, StateValueDataset):
            raise ValueError(
                "BestAcrossGenerationsRefiner is only implemented for StateValueDataset"
            )

        # ...and finally update the dataset by using the best value stored in the cache.
        self._merge_with_cache(dataset)
        return dataset


DatasetRefinerCreator = Callable[[DatasetRefiner], DatasetRefiner]
DatasetRefinerFactory = Callable[[], DatasetRefiner]


_DATASET_REFINER_CREATORS = {
    "reuse_refiner": ReuseRefiner,
    "best_across_generations_refiner": BestAcrossGenerationsRefiner,
}


def construct_dataset_refiner_factory(
    refiner_creators: List[DatasetRefinerCreator],
) -> DatasetRefinerFactory:
    """Composes a list of dataset refiner creators into one factory function."""

    def factory():
        combined_refiner = DatasetRefiner()

        for creator in refiner_creators:
            combined_refiner = creator(combined_refiner)

        return combined_refiner

    return factory


def get_dataset_refiner_creator_by_name(name: str,) -> DatasetRefinerCreator:
    return _DATASET_REFINER_CREATORS[name]


def get_available_dataset_refiners() -> List[str]:
    return list(_DATASET_REFINER_CREATORS.keys())
