# Copyright (c) 2020 Microsoft Corporation. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Jesse Michael Han, Daniel Selsam

import torch
import torch.nn as nn

class Reasoner(nn.Module):
    def __init__(self, cfg):
        super(Reasoner, self).__init__()
        self.cfg = cfg

    def forward(self, snapshot, choices):
        # snapshot :: R^{d}
        # choices :: R^{ncxd}
        # TODO(jesse):
        #   - repeat the snapshot and concat it with the choices
        #   - feed it through MLP to get the logits in R^{nc},
        raise Exception("Reasoner.forward not yet implemented")
