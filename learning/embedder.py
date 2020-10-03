# Copyright (c) 2020 Microsoft Corporation. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Sameera Lanka, Daniel Selsam

import torch
import torch.nn as nn

class Embedder(nn.Module):
    # TODO(sameera): this class is responsible for recursively embedding Embeddables.
    # Note: we make it a class even though it is "mostly" functional, in case we want
    # to add state, e.g. memoization for turning trees into DAGs.

    def __init__(self, cfg):
        super(Embedder, self).__init__()
        self.cfg = cfg

    def forward(self, embeddable):
        return self.embed(embeddable)

    def embed(self, embeddable):
        # Input: a term of Embeddable protobuf type (<repo>/protos/Embeddable.proto)
        # Output: a fixed dimensional embedding
        kind = embeddable.WhichOneof("body")
        if kind == "b":           return self.embed_bool(cmd.b)
        elif kind == "n":         return self.embed_int(cmd.n)
        elif kind == "s":         return self.embed_string(cmd.s)
        elif kind == "pair":      return self.embed_pair(cmd.pair)
        elif kind == "maybe":     return self.embed_maybe(cmd.maybe)
        elif kind == "lst":       return self.embed_list(cmd.list)
        elif kind == "array":     return self.embed_array(cmd.array)
        elif kind == "set":       return self.embed_set(cmd.set)
        elif kind == "map":       return self.embed_map(cmd.map)
        elif kind == "grid":      return self.embed_grid(cmd.grid)
        elif kind == "graph":     return self.embed_graph(cmd.graph)
        elif kind == "record":    return self.embed_record(cmd.record)
        else: raise Exception("[embed] invalid embeddable kind: %s" % kind)

    def embed_bool(self, b):
        # TODO(sameera): one vector for False, one for True
        raise Exception("embed_bool not yet implemented")

    def embed_int(self, n):
        # TODO(sameera): not sure best approach for ints
        # possibly just as sequence of tokens `int <digit>*`
        raise Exception("embed_int not yet implemented")

    def embed_string(self, s):
        # TODO(sameera): build vocabulary online, possibly splitting at whitespace
        raise Exception("embed_string not yet implemented")

    def embed_pair(self, pair):
        # TODO(sameera): could treat as `record "pair" [("fst", embed pair.1), ("snd", embed pair.2)]
        raise Exception("embed_pair not yet implemented")

    def embed_maybe(self, maybe):
        # TODO(sameera): baseline is vector for no-value, mlp applied to embedding of value otherwise
        raise Exception("embed_pair not yet implemented")

    def embed_list(self, list):
        # TODO(sameera): LSTM
        raise Exception("embed_list not yet implemented")

    def embed_array(self, array):
        # TODO(sameera): transformer with a reduce? 1-d convolution?
        raise Exception("embed_array not yet implemented")

    def embed_set(self, set):
        # TODO(sameera): something perm-invariant
        raise Exception("embed_set not yet implemented")

    def embed_map(self, map):
        # TODO(sameera): something perm-invariant
        raise Exception("embed_map not yet implemented")

    def embed_grid(self, grid):
        # TODO(sameera): CNN
        raise Exception("embed_grid not yet implemented")

    def embed_graph(self, graph):
        # TODO(sameera): GNN
        raise Exception("embed_graph not yet implemented")

    def embed_record(self, record):
        # TODO(sameera): simple tree
        # TODO(dselsam): distinguish between recursive and non-recursive records?
        raise Exception("embed_record not yet implemented")
