# Copyright (c) 2020 Microsoft Corporation. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Jesse Michael Han, Daniel Selsam

import torch
import torch.nn as nn
from learning.cnn import GridCNN
from learning.protos.Embeddable_pb2 import Embeddable
from learning.mlp import BasicMLP

def grid_idx(n_rows, n_cols):
    def idxer(idx):
        row_idx = int(math.floor(idx/n_rows))
        col_idx = idx % n_rows
        return row_idx, col_idx
    return idxer

class Embedder(nn.Module):
    # TODO(sameera): this class is responsible for recursively embedding Embeddables.
    # Note: we make it a class even though it is "mostly" functional, in case we want
    # to add state, e.g. memoization for turning trees into DAGs.
    def __init__(self, cfg):
        super(Embedder, self).__init__()
        self.cfg = cfg
        self.d = self.cfg["d"]
        self.grid_cnn = GridCNN(d_in=self.d, d_out=self.d, kernel_size=5) # TODO(jesse): don't hardcode
        self.list_lstm = nn.LSTM(input_size=self.d, hidden_size=self.d, batch_first=True)
        self.char_embedding = nn.Embedding(256, self.d)
        self.pair_mlp = BasicMLP(input_dim=2*self.d,
                                       hidden_dims=[2*self.d],
                                       output_dim=self.d,
                                       activation="leaky_relu",
                                       bias_at_end=True,
                                       p_dropout=0.0)

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
        return self.embed_string(str(n)) # >:(

    def embed_string(self, s): # TODO(dselsam, jesse): use token level embeddings!
        return self.embed_list_aux(s, (lambda c: self.char_embedding(ord(c))))

    def embed_char(self, c):
        return self.char_embedding(c)

    def embed_pair(self, pair):
        return self.pair_mlp(torch.cat([self.embed(pair.fst), self.embed(pair.snd)], dim=-1))

    def embed_maybe(self, maybe):
        # TODO(sameera): baseline is vector for no-value, mlp applied to embedding of value otherwise
        raise Exception("embed_maybe not yet implemented")

    def embed_list_aux(self, l, f):
        x = torch.empty(len(l), self.d)
        for i, elem in enumerate(l):
            x[i] = f(elem)
        return self.list_lstm(x.unsqueeze(0)).squeeze(0)

    def embed_list(self, l):
        return self.embed_list_aux(l.elems, f=self.embed)

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
        idxer = grid_idx(grid.n_rows, grid.n_cols)
        g = torch.empty(grid.n_rows, grid.n_cols, self.d)
        for idx, elem in grid.elems:
            i,j = idxer(idx)
            g[i,j] = self.embed(elem)
        g = g.view(self.d, grid.n_rows, grid.n_cols)
        return self.grid_cnn(g.unsqueeze(0)).squeeze(0)

    def embed_graph(self, graph):
        # TODO(sameera): GNN
        raise Exception("embed_graph not yet implemented")

    def embed_record(self, record):
        name_embedding = self.embed_string(record.name)
        fields_embedding = self.embed_list_aux(record.fields, f=self.embed_field)
        return self.pair_mlp(torch.cat([name_embedding, fields_embedding]))

    def embed_field(self, field):
        name_embedding = self.embed_string(field.name)
        value_embedding = self.embed(field.value)
        return self.pair_mlp(torch.cat([name_embedding, value_embedding], dim=-1))
