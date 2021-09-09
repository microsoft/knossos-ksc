# Import basic libraries
import numpy as np  # linear algebra
import pandas as pd  # data processing, CSV file I/O (e.g. pd.read_csv)
from collections import OrderedDict

# Import PyTorch
import torch  # import main library
from torch.autograd import Variable
import torch.nn as nn  # import modules
from torch.autograd import Function  # import Function to create custom activations
from torch.nn.parameter import (
    Parameter,
)  # import Parameter to create custom activations with learnable parameters
from torch import optim  # import optimizers for demonstrations
import torch.nn.functional as F  # import torch functions
from torchvision import datasets, transforms  # import transformations to use for demo

torch.manual_seed(1)

# Define a transform
transform = transforms.Compose([transforms.ToTensor()])

# Download and load the training data for Fashion MNIST
trainset = datasets.FashionMNIST(
    "~/.pytorch/F_MNIST_data/", download=True, train=True, transform=transform
)
trainloader = torch.utils.data.DataLoader(trainset, batch_size=64, shuffle=True)

# helper function to train a model
def train_model(model, trainloader):
    """
    Function trains the model and prints out the training log.
    INPUT:
        model - initialized PyTorch model ready for training.
        trainloader - PyTorch dataloader for training data.
    """
    # setup training

    # define loss function
    criterion = nn.NLLLoss()
    # define learning rate
    learning_rate = 0.003
    # define number of epochs
    epochs = 50
    # initialize optimizer
    optimizer = optim.Adam(model.parameters(), lr=learning_rate)

    # run training and print out the loss to make sure that we are actually fitting to the training set
    print("Training the model. Make sure that loss decreases after each epoch.\n")
    for e in range(epochs):
        running_loss = 0
        for images, labels in trainloader:
            images = images.view(images.shape[0], -1)
            log_ps = model(images)
            loss = criterion(log_ps, labels)

            optimizer.zero_grad()
            loss.backward()
            optimizer.step()

            running_loss += loss.item()
        else:
            # print out the loss to make sure it is decreasing
            print(f"Training loss: {running_loss}")


# initialize activation function
use_knossos = True
if use_knossos:
    import ksc.torch_frontend as knossos

    @knossos.elementwise
    def relu3(x: float) -> float:
        """
        Like ReLu, but smoother
        Like GeLu, but cheaper
        """
        if x < 0.0:
            return 0.0
        elif x < 1.0:
            return 1 / 3 * x ** 3
        else:
            return x - 2 / 3

    activation_function = relu3.nnModule(example_input=(torch.randn(3),))
else:

    def vrelu3_pytorch(x: torch.Tensor):
        mask1_inf = x > 1.0
        mask0_1 = (x > 0.0) & ~mask1_inf
        val_0_1 = 1 / 3 * x ** 3
        val_1_inf = x - 2 / 3

        return mask0_1 * val_0_1 + mask1_inf * val_1_inf

    activation_function = knossos.Lambda(vrelu3_pytorch)

# Initialize the model using nn.Sequential
model = nn.Sequential(
    OrderedDict(
        [
            ("fc1", nn.Linear(784, 256)),
            ("activation1", activation_function),
            ("fc2", nn.Linear(256, 128)),
            ("bn2", nn.BatchNorm1d(num_features=128)),
            ("activation2", activation_function),
            ("dropout", nn.Dropout(0.3)),
            ("fc3", nn.Linear(128, 64)),
            ("bn3", nn.BatchNorm1d(num_features=64)),
            ("activation3", activation_function),
            ("logits", nn.Linear(64, 10)),
            ("logsoftmax", nn.LogSoftmax(dim=1)),
        ]
    )
)

# Run training
train_model(model, trainloader)

