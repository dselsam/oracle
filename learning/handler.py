# Copyright (c) 2020 Microsoft Corporation. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Daniel Selsam

from learning.protos.Response_pb2 import Response, Prediction
from model import GenericModel

class Handler:
    def __init__(self, cfg):
        # TODO(sameera): store model (+ friends) here
        self.model = GenericModel(cfg['model'])

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
        # TODO(sameera): re-initialize model (+ friends)
        response = Response()
        response.success = False
        response.msg     = "init command not yet implemented"
        return response

    def handle_predict(self, predict_cmd):
        # TODO(sameera): map predict.choicePoints to Response.predictions
        response             = Response()
        response.success     = False
        response.msg         = "predict command not yet implemented"
        for choicePoint in predict_cmd.choicePoints:
            # TODO(dselsam): change name to prediction
            prediction = Prediction()
            denom = len(choicePoint.choices)
            prediction.policy.extend([1.0 / denom for _ in choicePoint.choices])
            response.predictions.append(prediction)
        return response

    def handle_train(self, train_cmd):
        # TODO(sameera): train train_cmd.nEpochs epochs on train_cmd.datapoints, return average loss
        # (using current model)
        response = Response()
        response.success = False
        response.msg     = "train command not yet implemented"
        response.loss    = 0.0
        return response

    def handle_valid(self, valid_cmd):
        # TODO(sameera): return average loss
        # (using current model)
        response = Response()
        response.success = False
        response.msg     = "valid command not yet implemented"
        response.loss    = 0.0
        return response

    def handle_save(self, save_cmd):
        # TODO(sameera): store all relevant data to save_cmd.filename
        response = Response()
        response.success = False
        response.msg     = "save command not yet implemented"
        return response

    def handle_load(self, load_cmd):
        # TODO(sameera): load all relevant data to load_cmd.filename
        response = Response()
        response.success = False
        response.msg     = "load command not yet implemented"
        return response
