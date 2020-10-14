# Copyright (c) 2020 Microsoft Corporation. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Sameera Lanka, Jesse Michael Han, Daniel Selsam

import torch
import torch.nn as nn
from models.embedder import Embedder
from models.reasoner import Reasoner


class GenericModel(nn.Module):
    def __init__(self, cfg):
        super(GenericModel, self).__init__()
        self.cfg = cfg
        self.embedder = Embedder(cfg['embedder'])
        self.reasoner = Reasoner(cfg['reasoner'])

    def forward(self, snapshot, choices):
        snapshot_embedding = self.embedder(snapshot)
        choices_embeddings = torch.cat([self.embedder(choice).unsqueeze(1) for choice in choices],
                                       dim=1)
        # reasoner is responsible for tiling snapshot and concatenating
        return self.reasoner(snapshot_embedding, choices_embeddings)
