# Copyright (c) 2020 Microsoft Corporation. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Jesse Michael Han, Daniel Selsam

import torch
import torch.nn as nn

from embedder import Embedder
from reasoner import Reasoner

class GenericModel(nn.Module):
    def __init__(self, cfg):
        super(GenericModel, self).__init__()
        self.cfg = cfg
        self.embedder = Embedder(cfg['embedder'])
        self.reasoner = Reasoner(cfg['reasoner'])

        self.add_module("embedder", self.embedder)
        self.add_module("reasoner", self.reasoner)

        # TODO: tmp only, until the other modules have parameters
        self.add_module("tmp_linear", nn.Linear(1, 1))

    def forward(self, snapshot, choices):
        # TODO(jesse):
        #   - pipe embedder into reasoner
        # e_snapshot  = self.embedder(snapshot)
        # e_choices   = self.embedder(choices)
        return torch.zeros(len(choices))
