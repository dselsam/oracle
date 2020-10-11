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
        snapshot_embedding = self.embedder(snapshot)
        choices_embedding = torch.empty(len(choices), self.embedder.d)
        for i, choice in enumerate(choices):
            choices_embedding[i] = self.embedder(choice)            
        # reasoner is responsible for tiling snapshot and concatenating
        return self.reasoner(snapshot_embedding, choices_embedding)
