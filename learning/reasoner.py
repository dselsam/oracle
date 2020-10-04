# Copyright (c) 2020 Microsoft Corporation. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Jesse Michael Han, Daniel Selsam

import torch
import torch.nn as nn
from mlp import BasicMLP

class Reasoner(nn.Module):
    def __init__(self, cfg):
        super(Reasoner, self).__init__()
        self.cfg = cfg
        self.mlp = BasicMLP(input_dim=2*cfg["d"],
                            hidden_dims=[2*cfg["d"] for _ in range(cfg["n_layers"])],
                            output_dim=cfg["d"],
                            activation="leaky_relu",
                            bias_at_end=True,
                            p_dropout=0.0)

    def forward(self, snapshot, choices):
        # snapshot :: R^{d}
        # choices :: R^{ncxd}
        snapshot = snapshot.unsqueeze(0).repeat(len(choices), 1)
        x = torch.cat([snapshot, choices], dim=-1)
        return self.mlp(x)
