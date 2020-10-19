# Copyright (c) 2020 Microsoft Corporation. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Sameera Lanka, Jesse Michael Han, Daniel Selsam

import torch
import torch.nn as nn
from typing import List, Any, Set
from models.cnn import GridCNN
from protos.Embeddable_pb2 import Embeddable
from models.feedforward import FeedForwardNet
from models.transformer import TextTransformer
from transformers import GPT2Tokenizer
from dataclasses import dataclass


class Embedder(nn.Module):
    def __init__(self, cfg):
        super(Embedder, self).__init__()
        self.cfg = cfg
        self.d = cfg['d']

        ## Bool ##
        self.bool_embedding = nn.Embedding(2, self.d)


        ## Int ##
        self.int_embedding = nn.Embedding(10, self.d)

        ## String ##
        # Cannot construct vocab online due to initialization of embedding layer
        string_cfg = cfg['TextTF']
        self.tokenizer = GPT2Tokenizer.from_pretrained('gpt2', cache_dir='./learning/models/tokenizer')
        special_tokens = {'bos_token': '<s>',
                          'eos_token': '</s>',
                          'pad_token': '<pad>'
                          }
        self.tokenizer.add_special_tokens(special_tokens)
        self.text_embedding = nn.Embedding(len(self.tokenizer), self.d, padding_idx=self.tokenizer.pad_token_id)
        self.text_encoder = TextTransformer(text_embed_dim=self.d,
                                            ff_dim=string_cfg['ff_dim'],
                                            n_heads=string_cfg['n_heads'],
                                            n_layers=string_cfg['n_layers'],
                                            dropout=string_cfg['p_dropout'])

        ## Grid ##
        grid_cfg = cfg['GridCNN']
        self.grid_encoder = GridCNN(n_rows=grid_cfg['n_rows'],
                                    n_cols=grid_cfg['n_cols'],
                                    input_dim=self.d,
                                    hidden_dims=[self.d for _ in range(grid_cfg['n_conv_layers'])],
                                    output_dim=self.d,
                                    kernel_size=grid_cfg['kernel_size'],
                                    activation=grid_cfg['activation'],
                                    add_channel_pool=grid_cfg['add_channel_pool'])

        ## List ##
        list_cfg = cfg['ListLSTM']
        bidirectional = list_cfg['bidirectional']
        self.list_encoder = nn.LSTM(bidirectional=bidirectional,
                                    input_size=self.d,
                                    hidden_size=self.d // 2 if bidirectional else self.d,
                                    batch_first=True,
                                    num_layers=list_cfg['n_layers'],
                                    dropout=list_cfg['p_dropout'])

        ## Pair ##
        pair_cfg = cfg['PairFF']
        self.pair_encoder = FeedForwardNet(input_dim=2 * self.d,
                                           hidden_dims=[self.d for _ in range(pair_cfg['n_layers'])],
                                           output_dim=self.d,
                                           activation=pair_cfg['activation'],
                                           p_dropout=pair_cfg['p_dropout'])

        ## Set ##
        set_cfg = cfg['SetFF']
        self.set_encoder = FeedForwardNet(input_dim=self.d,
                                          hidden_dims=[self.d for _ in range(set_cfg['n_layers'])],
                                          output_dim=self.d,
                                          activation=set_cfg['activation'],
                                          p_dropout=set_cfg['p_dropout'])

        self.list_nil = torch.nn.Parameter(torch.nn.init.xavier_normal_(torch.empty(1, self.d)))

    def forward(self, embeddable: List[Embeddable]):
        return self.embed(embeddable)

    def embed(self, embeddable):
        # Input: a term of Embeddable protobuf type (<repo>/protos/Embeddable.proto)
        # Output: a fixed dimensional embedding

        if isinstance(embeddable, torch.Tensor) and embeddable.size(-1) == self.d:
            return embeddable  # already encoded
        kind = embeddable.WhichOneof("body")
        if kind == "b":
            result = self.embed_bool(embeddable.b)
        elif kind == "char":
            result = self.embed_char(embeddable.char)
        elif kind == "n":
            result = self.embed_int(embeddable.n)
        elif kind == "s":
            result = self.embed_string(embeddable.s)
        elif kind == "pair":
            result = self.embed_pair(embeddable.pair)
        elif kind == "maybe":
            result = self.embed_maybe(embeddable.maybe)
        elif kind == "list":
            result = self.embed_list(embeddable.list)
        elif kind == "array":
            result = self.embed_array(embeddable.array)
        elif kind == "set":
            result = self.embed_set(embeddable.set)
        elif kind == "map":
            result = self.embed_map(embeddable.map)
        elif kind == "grid":
            result = self.embed_grid(embeddable.grid)
        elif kind == "graph":
            result = self.embed_graph(embeddable.graph)
        elif kind == "record":
            result = self.embed_record(embeddable.record)
        else:
            raise Exception("[embed] invalid embeddable kind: %s" % kind)

        try:
            assert result.size(-1) == self.d
            # assert len(result.size()) == 1
        except AssertionError as e:
            print("BAD RESULT: ", result)
            print("BAD RESULT SIZE: ", result.size())
            print("BAD RESULT EMBEDDABLE KIND: ", kind)
            raise e
        return result

    def embed_bool(self, b: List[bool]):
        b = torch.as_tensor([b]).int().to(self.device)
        return self.bool_embedding(b)

    def embed_int(self, n: List[int]):
        # int_embedding = torch.as_tensor([[n]]).float().to(self.device)
        n = torch.as_tensor([n]).to(self.device)
        return self.int_embedding(n)
        # return self._zero_pad_1d(int_embedding)

    def embed_char(self, c: List):
        if isinstance(c, int):
            return self.embed_string(chr(c))
        return self.embed_string(c)

    def embed_string(self, s: List[str]):
        # should we use LSTM instead?
        tokens = self.tokenizer(s).input_ids
        pad_token = self.tokenizer.pad_token_id
        # max_seq_len = max([len(tokens_i) for tokens_i in tokens])  # when we move on to batching
        # input_tokens = torch.as_tensor(tokens + [pad_token] * (max_seq_len - len(tokens))).to(self.device)
        tokens_tensor = torch.as_tensor([tokens]).to(self.device)
        input_embedding = self.text_embedding(tokens_tensor)
        if len(s) == 1:
            return input_embedding.squeeze(0)
        return self.list_encoder(input_embedding)[0][:, -1, :]
        # pad_mask = (tokens_tensor == pad_token)
        # return self.text_encoder(input_embedding, pad_mask)

    def embed_pair(self, pair: List):
        pair_embedding = torch.cat([self.embed(pair.fst),
                                    self.embed(pair.snd)], dim=-1)
        return self.pair_encoder(pair_embedding)

    def embed_maybe(self, maybe):
        # TODO: baseline is vector for no-value, mlp applied to embedding of value otherwise
        raise Exception("embed_maybe not yet implemented")

    def embed_list(self, l: List[Any]):
        if not l.elems:
            return self.list_nil
        list_elem_embeddings = torch.cat([self.embed(elem).unsqueeze(1) for elem in l.elems], dim=1)
        return self.list_encoder(list_elem_embeddings)[0][:, -1, :]

    def embed_array(self, array):
        # what is the diff between array/list?
        return self.embed_list(array)

    def embed_set(self, s):
        set_elem_embeddings = torch.stack([self.embed(elem) for elem in s.elems])
        set_embedding = set_elem_embeddings.mean(dim=0)  # alternative poolings: max, sum
        return self.set_encoder(set_embedding)

    def embed_map(self, m):
        # redo
        @dataclass
        class SetMessage:
            elems: Set[Any]

        set_elems = set([self.embed_pair(assoc) for assoc in m.assocs])
        return self.embed_set(SetMessage(set_elems))

    def embed_grid(self, grid):
        grid_elem_embeddings = torch.stack([self.embed(elem) for elem in grid.elems])
        grid_embedding = grid_elem_embeddings.view(1, -1, grid.nRows, grid.nCols)
        return self.grid_encoder(grid_embedding)

    def embed_graph(self, graph):
        # TODO: GNN
        raise Exception("embed_graph not yet implemented")

    def embed_record(self, record):
        # This should ideally be:
        #    return self.embed(Pair(record.name, record.fields))
        @dataclass
        class ListMessage:
            elems: List[Any]

        @dataclass
        class PairMessage:
            fst: Any
            snd: Any

        name_embedding = self.embed_string(record.name)
        # this should automatically recognize that fields is a list
        fields_embedding = self.embed_list(ListMessage([self.embed_field(field) for field in record.fields]))
        return self.embed_pair(PairMessage(name_embedding, fields_embedding))

    def embed_field(self, field):  # should we add fields as a type in embed()?
        # Redefine as a pair? see embed_record
        # Also, if value is a float, we need embed_float
        name_embedding = self.embed_string(field.name)
        value_embedding = self.embed(field.value)
        return self.pair_encoder(torch.cat([name_embedding, value_embedding], dim=-1))

    def _zero_pad_1d(self, embedding: torch.FloatTensor):
        """Pads a 1-D tensor with zeros to the right"""
        batch_size = embedding.size(0)
        padding = torch.zeros(batch_size, self.d - embedding.size(-1), device=self.device)
        return torch.cat([embedding, padding], dim=-1)

    def _zero_pad_2d(self, embedding):
        raise NotImplementedError

    @property
    def device(self):
        return next(self.parameters()).device
