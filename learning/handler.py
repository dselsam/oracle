# Copyright (c) 2020 Microsoft Corporation. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Daniel Selsam

from protos.Response_pb2 import Response, Prediction
from model import GenericModel

import torch
import torch.nn as nn
import torch.optim as optim

def count_params(model):
    count = 0
    params = list(model.parameters())
    for p in params:
        if p.requires_grad:
            count += p.numel()
    return count

def unpack_datapoint(datapoint):
    snapshot    = datapoint.choicepoint.snapshot
    choices     = datapoint.choicepoint.choices
    choiceIdx   = datapoint.label.choiceIdx
    return snapshot, choices, torch.as_tensor([choiceIdx])

class Handler:
    def __init__(self, cfg):
        self.cfg       = cfg
        self.model     = GenericModel(cfg['model'])
        self.loss      = nn.CrossEntropyLoss()
        self.optimizer = optim.Adam(self.model.parameters(), lr=cfg['optim']['learning_rate'])

    def handle(self, cmd):
        kind = cmd.WhichOneof("body")
        if kind == "init":        return self.handle_init(cmd.init)
        elif kind == "predict":   return self.handle_predict(cmd.predict)
        elif kind == "train":     return self.handle_train(cmd.train)
        elif kind == "valid":     return self.handle_valid(cmd.valid)
        elif kind == "save":      return self.handle_save(cmd.save)
        elif kind == "load":      return self.handle_load(cmd.load)
        else: raise Exception("[handle] invalid cmd kind: %s" % kind)

    def handle_init(self, init_cmd):
        # TODO: re-initialize model (+ friends)
        response = Response()
        response.success = False
        response.msg     = "init command not yet implemented"
        return response

    def handle_predict(self, predict_cmd):
        response             = Response()
        response.success     = False
        response.msg         = "predict command not yet implemented"
        for choicepoint in predict_cmd.choicepoints:
            with torch.set_grad_enabled(False):
                logits = self.model(choicepoint.snapshot, choicepoint.choices)
                policy = nn.functional.softmax(logits, dim=-1)
                prediction = Prediction()
                prediction.policy.extend(list(policy.squeeze(0)))
                response.predictions.append(prediction)

        response.success = True
        return response

    def handle_train(self, train_cmd):
        total_loss = 0.0
        n_steps    = 0
        for _ in range(train_cmd.nEpochs):
            for datapoint in train_cmd.datapoints:
                snapshot, choices, choiceIdx = unpack_datapoint(datapoint)
                with torch.set_grad_enabled(True):
                    logits = self.model(snapshot, choices)
                    loss   = self.loss(logits, choiceIdx)

                self.model.zero_grad()
                loss.backward()
                torch.nn.utils.clip_grad_norm_(self.model.parameters(), self.cfg['optim']['grad_norm_clip'])
                self.optimizer.step()
                total_loss += loss
                n_steps += 1

        response = Response()
        response.loss    = total_loss / n_steps if n_steps > 0 else float('nan')
        response.success = True
        return response

    def handle_valid(self, valid_cmd):
        total_loss = 0.0
        n_steps    = 0
        for datapoint in valid_cmd.datapoints:
            snapshot, choices, choiceIdx = unpack_datapoint(datapoint)
            with torch.set_grad_enabled(False):
                logits = self.model(snapshot, choices)
                loss   = self.loss(logits, choiceIdx)
            total_loss += loss
            n_steps += 1

        response = Response()
        response.loss    = total_loss / n_steps if n_steps > 0 else float('nan')
        response.success = True
        return response

    def handle_save(self, save_cmd):
        # TODO: store all relevant data to save_cmd.filename
        response = Response()
        response.msg     = "save command not yet implemented"
        response.success = False
        return response

    def handle_load(self, load_cmd):
        # TODO: load all relevant data to load_cmd.filename
        response = Response()
        response.msg     = "load command not yet implemented"
        response.success = False
        return response
