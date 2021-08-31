import os
import pickle

from rlo import analytics
from rlo.dataset_refiner import DatasetRefiner
from rlo.search_ops import SearcherState
from rlo import utils


class TrainingRunState:
    def __init__(
        self,
        searcher_state: SearcherState,
        dataset_refiner: DatasetRefiner,
        generation: int,
        total_train_time: float,
    ):
        self._searcher_state = searcher_state
        self._dataset_refiner = dataset_refiner
        self._generation = generation
        self._total_train_time = total_train_time

    def save(self, model_save_dir):
        save_dir = utils.get_model_save_path(model_save_dir, self._generation)
        with open(os.path.join(save_dir, "saved_state.pck"), "wb") as f:
            pickle.dump(self, f)

    @staticmethod
    def try_load(model_save_dir, generation):
        saved_state_file = os.path.join(
            utils.get_model_save_path(model_save_dir, generation), "saved_state.pck"
        )
        if not os.path.isfile(saved_state_file):
            return None
        analytics.event(
            "restored_state", generation=generation, saved_state_file=saved_state_file
        )
        saved_state = utils.load_pickle(saved_state_file)
        assert isinstance(saved_state, TrainingRunState)
        assert saved_state.generation == generation
        return saved_state

    @property
    def generation(self):
        return self._generation

    @property
    def searcher_state(self):
        # Note that unfortunately this is not immutable
        return self._searcher_state

    @property
    def dataset_refiner(self):
        # Note that unfortunately this is not immutable
        return self._dataset_refiner

    @property
    def total_train_time(self):
        return self._total_train_time
