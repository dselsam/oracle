# Copyright (c) 2020 Microsoft Corporation. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Sameera Lanka, Jesse Michael Han

import torch.nn as nn


class GridCNN(nn.Module):
    def __init__(self, n_rows, n_cols, input_dim, hidden_dims, output_dim, kernel_size, add_channel_pool: False):
        super(GridCNN, self).__init__()
        padding = (kernel_size - 1) // 2
        layer_dims = [input_dim] + hidden_dims
        self.flat_feature_dim = n_rows * n_cols * layer_dims[-1]
        conv_layers = [x for i in range(len(layer_dims) - 1)
                       for x in (nn.Conv2d(layer_dims[i], layer_dims[i+1], kernel_size=kernel_size, padding=padding),
                                 nn.LeakyReLU())
                       ]
        if add_channel_pool:
            conv_layers.extend([nn.Conv2d(layer_dims[-1], layer_dims[-1], kernel_size=1),
                                nn.LeakyReLU()])
        self.conv_layers = nn.Sequential(*conv_layers)
        self.linear = nn.Linear(self.flat_feature_dim, output_dim)

    def forward(self, x):
        x = self.conv_layers(x)
        out = self.linear(x.view(-1, self.flat_feature_dim))
        return out
