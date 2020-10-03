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

    def forward(self, snapshot, choices):
        # TODO(jesse):
        #   - pipe embedder into reasoner
        raise Exception("GenericModel.forward not yet implemented")
