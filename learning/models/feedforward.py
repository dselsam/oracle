# Copyright (c) 2020 Microsoft Corporation. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Sameera Lanka, Jesse Michael Han

import torch.nn as nn
from typing import List

activation_fn = {
    'relu': nn.ReLU(),
    'leaky_relu': nn.LeakyReLU(),
    'relu6': nn.ReLU6(),
    'elu': nn.ELU()
}


class FeedForwardNet(nn.Module):
    def __init__(self, input_dim: int, hidden_dims: List[int], output_dim: int, activation: str, final_bias=True,
                 p_dropout=0.0):
        super(FeedForwardNet, self).__init__()
        layer_dims = [input_dim] + hidden_dims
        self.layers = nn.ModuleList([nn.Linear(layer_dims[i], layer_dims[i + 1]) for i in range(len(layer_dims) - 1)])
        self.layers.append(nn.Linear(layer_dims[-1], output_dim, bias=final_bias))
        self.dropout = nn.Dropout(p_dropout)
        self.activation = activation_fn[activation]

    def forward(self, x):
        for l in self.layers[:-1]:
            x = self.activation(l(x))
            x = self.dropout(x)
        out = self.layers[-1](x)
        return out
