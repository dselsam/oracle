# Copyright (c) 2020 Microsoft Corporation. All rights reserved.
# Released under Apache 2.0 license as described in the file LICENSE.
# Authors: Sameera Lanka


import math
import torch
import torch.nn as nn


class PositionalEncoding(nn.Module):
    def __init__(self, d_model, dropout=0.1, max_len=5000):
        super(PositionalEncoding, self).__init__()
        self.dropout = nn.Dropout(p=dropout)
        pe = torch.zeros(max_len, d_model)
        position = torch.arange(0, max_len, dtype=torch.float).unsqueeze(1)
        div_term = torch.exp(torch.arange(0, d_model, 2).float() * (-math.log(10000.0) / d_model))
        pe[:, 0::2] = torch.sin(position * div_term)
        pe[:, 1::2] = torch.cos(position * div_term)
        pe = pe.unsqueeze(0)
        self.register_buffer('pe', pe)

    def forward(self, x):
        x = x + self.pe[:, :x.size(1)]
        return self.dropout(x)


class TextTransformer(nn.Module):
    def __init__(self, text_embed_dim, ff_dim, n_heads, n_layers, dropout=0.0):
        super(TextTransformer, self).__init__()
        self.positional_encoding = PositionalEncoding(d_model=text_embed_dim, dropout=dropout)
        enc_layer = nn.TransformerEncoderLayer(d_model=text_embed_dim, dim_feedforward=ff_dim, nhead=n_heads)
        self.encoder = nn.TransformerEncoder(enc_layer, num_layers=n_layers)

    def encode(self, input_embedding, mask):
        embed = input_embedding * math.sqrt(self.text_embed_dim)
        embed = self.positional_encoding(embed)
        # pt transformer input must be of shape (seq_len, batch_size, ..) - inelegant
        embed = self.encoder(embed.permute(1, 0, 2), src_key_padding_mask=mask).permute(1, 0, 2)
        return embed.mean(1)

    def forward(self, input_ids, input_pad_mask):
        return self.encode(input_ids, input_pad_mask)
