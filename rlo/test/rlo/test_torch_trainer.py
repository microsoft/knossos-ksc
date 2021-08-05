import torch

from rlo.training import TrainingConfig, train_model
from rlo.model.model import TorchModelWrapper, Model
from rlo.torch_graph_data import DataConverter, BatchedGraphData
from rlo import utils


from mock_dataset import mock_state_value_dataset


class DummyStateValueModel(Model):
    """A dummy state-value model which returns the same value for every input.
    """

    def __init__(self, *, num_time_heads: int):
        super().__init__()
        self.num_time_heads = num_time_heads
        self._param = torch.nn.Parameter(torch.rand(1,))

    def init_optimizer_and_loss(self):
        self._optimizer = torch.optim.SGD(params=self.parameters(), lr=0.1)
        self._loss_func = lambda predictions, target, weight: torch.mean(
            (predictions - target) ** 2 * weight
        )

    def forward(self, batch: BatchedGraphData) -> torch.Tensor:
        batch_size = batch.num_graphs
        return (
            torch.zeros(batch_size, self.num_time_heads, device=self._param.device)
            + self._param
        )


def test_fit_dummy_model():
    config = TrainingConfig(min_epochs=3, max_epochs=10, batch_size=3)
    num_time_heads = 3
    with utils.random_state_context(10):
        model = DummyStateValueModel(num_time_heads=num_time_heads)
    model_wrapper = TorchModelWrapper(
        model=model,
        data_converter=DataConverter(
            num_time_heads=num_time_heads,
            cost_norm="none",
            use_subtree_match_edges=False,
            device=torch.device("cpu"),
        ),
        device=torch.device("cpu"),
    )
    dataset = mock_state_value_dataset()
    train_model(config=config, model_wrapper=model_wrapper, dataset=dataset, seed=0)
