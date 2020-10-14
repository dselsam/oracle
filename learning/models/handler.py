# Copyright (c) 2020 Microsoft Corporation. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Sameera Lanka, Daniel Selsam

from protos.Response_pb2 import Response, Prediction
from models.model import GenericModel

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
    snapshot = datapoint.choicepoint.snapshot
    choices = datapoint.choicepoint.choices
    choice_id = datapoint.label.choiceIdx
    return snapshot, choices, torch.as_tensor([choice_id])


class Handler:
    def __init__(self, cfg):
        self.model_cfg = cfg['model']
        self.optim_cfg = cfg['optim']
        self.model = GenericModel(self.model_cfg)
        self.loss = nn.NLLLoss()  # can update to KLDivLoss
        self.optimizer = optim.Adam(self.model.parameters(), lr=self.optim_cfg['learning_rate'])

    def handle(self, cmd):
        kind = cmd.WhichOneof("body")
        if kind == "init":
            return self.handle_init(cmd.init)
        elif kind == "predict":
            return self.handle_predict(cmd.predict)
        elif kind == "train":
            return self.handle_train(cmd.train)
        elif kind == "valid":
            return self.handle_valid(cmd.valid)
        elif kind == "save":
            return self.handle_save(cmd.save)
        elif kind == "load":
            return self.handle_load(cmd.load)
        else:
            raise Exception("[handle] invalid cmd kind: %s" % kind)

    def handle_init(self, init_cmd):
        response = Response()
        response.success = False
        self.model = GenericModel(self.model_cfg)
        self.optimizer = optim.Adam(self.model.parameters(), lr=self.optim_cfg['learning_rate'])
        response.msg = "Model and optimizer reinitialized"
        response.success = True
        return response

    def handle_predict(self, predict_cmd):
        response = Response()
        response.success = False
        self.model.eval()
        for choicepoint in predict_cmd.choicepoints:
            with torch.set_grad_enabled(False):
                log_prob = self.model(choicepoint.snapshot, choicepoint.choices)
                prediction = log_prob.argmax(dim=-1)
                response.predictions.append(prediction)

        response.success = True
        return response

    def handle_train(self, train_cmd):
        total_loss = 0.0
        n_steps = 0
        self.model.train()
        for _ in range(train_cmd.nEpochs):
            for datapoint in train_cmd.datapoints:
                snapshot, choices, choice_id = unpack_datapoint(datapoint)
                with torch.set_grad_enabled(True):
                    log_prob = self.model(snapshot, choices)
                    loss = self.loss(log_prob, choice_id)

                self.model.zero_grad()
                loss.backward()
                torch.nn.utils.clip_grad_norm_(self.model.parameters(), self.optim_cfg['grad_norm_clip'])
                self.optimizer.step()
                total_loss += loss
                n_steps += 1

        response = Response()
        response.loss = total_loss / n_steps if n_steps > 0 else float('nan')
        response.success = True
        return response

    def handle_valid(self, valid_cmd):
        total_loss = 0.0
        n_steps = 0
        self.model.eval()
        for datapoint in valid_cmd.datapoints:
            snapshot, choices, choice_id = unpack_datapoint(datapoint)
            with torch.set_grad_enabled(False):
                logits = self.model(snapshot, choices)
                loss = self.loss(logits, choice_id)
            total_loss += loss
            n_steps += 1

        response = Response()
        response.loss = total_loss / n_steps if n_steps > 0 else float('nan')
        response.success = True
        return response

    def handle_save(self, save_cmd):
        response = Response()
        checkpoint = {
            'model_state_dict': self.model.state_dict(),
            'optimizer_state_dict': self.optimizer.state_dict()
        }
        torch.save(checkpoint, save_cmd.filename)
        response.msg = f"Model and optimizer saved at {save_cmd.filename}"
        response.success = True
        return response

    def handle_load(self, load_cmd):
        response = Response()
        checkpoint = torch.load(load_cmd.filename)
        self.model.load_state_dict(checkpoint['model_state_dict'])
        self.optimizer.load_state_dict(checkpoint['optimizer_state_dict'])
        response.msg = "Model and optimizer loaded"
        response.success = True
        return response
