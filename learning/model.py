# Copyright (c) 2020 Microsoft Corporation. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Jesse Michael Han, Daniel Selsam

import torch
import torch.nn as nn

from embed import Embedder
from reasoner import Reasoner

class UniversalOracle(nn.Module):
    def __init__(self, cfg):
        super(UniversalOracle, self).__init__()
        self.cfg = cfg
        self.embedder = Embedder(cfg)
        self.reasoner = Reasoner(cfg)

    def forward(self, snapshot, choices):
        # TODO(jesse):
        #   - pipe embedder into reasoner
        raise Exception("UniversalOracle.forward not yet implemented")
