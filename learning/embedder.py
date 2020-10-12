# Copyright (c) 2020 Microsoft Corporation. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Jesse Michael Han, Daniel Selsam

import torch
import torch.nn as nn
import math
from cnn import GridCNN
from protos.Embeddable_pb2 import Embeddable
from mlp import BasicMLP

def grid_idx(n_rows, n_cols):
    def idxer(idx):
        row_idx = int(math.floor(idx/n_rows))
        col_idx = idx % n_rows
        return row_idx, col_idx
    return idxer

class Embedder(nn.Module):
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
        self.list_nil = torch.nn.Parameter(torch.nn.init.xavier_normal_(torch.empty([1,self.d])), requires_grad=True).view(self.d)

    def forward(self, embeddable):
        return self.embed(embeddable)

    def embed(self, embeddable):
        # Input: a term of Embeddable protobuf type (<repo>/protos/Embeddable.proto)
        # Output: a fixed dimensional embedding
        kind = embeddable.WhichOneof("body")
        if kind == "b":           result = self.embed_bool(embeddable.b)
        elif kind == "char":      result = self.embed_char(embeddable.char)
        elif kind == "n":         result = self.embed_int(embeddable.n)
        elif kind == "s":         result = self.embed_string(embeddable.s)
        elif kind == "pair":      result = self.embed_pair(embeddable.pair)
        elif kind == "maybe":     result = self.embed_maybe(embeddable.maybe)
        elif kind == "list":      result = self.embed_list(embeddable.list)
        elif kind == "array":     result = self.embed_array(embeddable.array)
        elif kind == "set":       result = self.embed_set(embeddable.set)
        elif kind == "map":       result = self.embed_map(embeddable.map)
        elif kind == "grid":      result = self.embed_grid(embeddable.grid)
        elif kind == "graph":     result = self.embed_graph(embeddable.graph)
        elif kind == "record":    result = self.embed_record(embeddable.record)
        else: raise Exception("[embed] invalid embeddable kind: %s" % kind)

        try:
            assert result.size(0) == self.d
            assert len(result.size()) == 1
        except AssertionError as e:
            print("BAD RESULT: ", result)
            print("BAD RESULT SIZE: ", result.size())
            print("BAD RESULT EMBEDDABLE KIND: ", kind)
            raise e
        return result

    def embed_bool(self, b):
        # TODO: one vector for False, one for True
        raise Exception("embed_bool not yet implemented")

    def embed_int(self, n):
        # TODO: special MLP for this?
        return self.embed_string(str(n)) # >:(

    def embed_string(self, s):
        # TODO: use token level embeddings!
        return self.embed_list_aux(s, (lambda c: self.char_embedding(torch.as_tensor([ord(c)]))))

    def embed_char(self, c):
        return self.char_embedding(torch.as_tensor(c))

    def embed_pair(self, pair):
        return self.pair_mlp(torch.cat([self.embed(pair.fst), self.embed(pair.snd)], dim=-1))

    def embed_maybe(self, maybe):
        # TODO: baseline is vector for no-value, mlp applied to embedding of value otherwise
        raise Exception("embed_maybe not yet implemented")

    def embed_list_aux(self, l, f):
        if len(l) == 0:
            return self.list_nil

        x = torch.empty(len(l), self.d)
        for i, elem in enumerate(l):
            x[i] = f(elem)
        return self.list_lstm(x.unsqueeze(0))[1][0].squeeze(0).squeeze(0)

    def embed_list(self, l):
        return self.embed_list_aux(l.elems, f=self.embed)

    def embed_array(self, array):
        # TODO: transformer with a reduce? 1-d convolution?
        raise Exception("embed_array not yet implemented")

    def embed_set(self, set):
        # TODO: something perm-invariant
        raise Exception("embed_set not yet implemented")

    def embed_map(self, map):
        # TODO: something perm-invariant
        raise Exception("embed_map not yet implemented")

    def embed_grid(self, grid):
        idxer = grid_idx(grid.nRows, grid.nCols)
        g = torch.empty(grid.nRows, grid.nCols, self.d)
        for idx, elem in enumerate(grid.elems):
            i,j = idxer(idx)
            g[i,j] = self.embed(elem)
        g = g.view(self.d, grid.nRows, grid.nCols)
        return self.grid_cnn(g.unsqueeze(0)).view(self.d)

    def embed_graph(self, graph):
        # TODO: GNN
        raise Exception("embed_graph not yet implemented")

    def embed_record(self, record):
        name_embedding   = self.embed_string(record.name)
        fields_embedding = self.embed_list_aux(record.fields, f=self.embed_field)
        return self.pair_mlp(torch.cat([name_embedding, fields_embedding], dim=-1))

    def embed_field(self, field):
        name_embedding  = self.embed_string(field.name)
        value_embedding = self.embed(field.value)
        return self.pair_mlp(torch.cat([name_embedding, value_embedding], dim=-1))
