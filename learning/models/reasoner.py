# Copyright (c) 2020 Microsoft Corporation. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Sameera Lanka, Jesse Michael Han, Daniel Selsam

import torch
import torch.nn as nn
from models.feedforward import FeedForwardNet


class Reasoner(nn.Module):
    def __init__(self, cfg):
        super(Reasoner, self).__init__()
        self.d = cfg['d']
        self.ff = FeedForwardNet(input_dim=2 * self.d,
                                 hidden_dims=[2 * self.d for _ in range(cfg["n_layers"])],
                                 output_dim=1,
                                 activation="leaky_relu",
                                 final_bias=False,
                                 p_dropout=0.0)
        self.log_softmax = nn.LogSoftmax(dim=-1)

    def forward(self, snapshot, choices):
        # snapshot :: R^{d}  batch_size, self.d
        # choices :: R^{ncxd} batch_size, n_choices, self.d
        snapshot = snapshot.unsqueeze(1).expand_as(choices)
        x = torch.cat([snapshot, choices], dim=-1)
        out = self.log_softmax(self.ff(x))  # batch_size, n_choices, 1
        return out.squeeze(-1)
