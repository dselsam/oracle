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


class TextEncoder(nn.Module):
    def __init__(self, vocab_size, embed_dim, feedforward_dim,
                 snapshot_num_heads, snapshot_num_layers,
                 choice_num_heads, choice_num_layers,
                 dropout=0.1, pad_token_id=None):

        super(TextEncoder, self).__init__()
        self.embed_dim = embed_dim
        self.embedding = nn.Embedding(vocab_size, embed_dim, padding_idx=pad_token_id)
        self.positional_encoding = PositionalEncoding(d_model=embed_dim, dropout=dropout)
        snapshot_layer = nn.TransformerEncoderLayer(d_model=embed_dim, dim_feedforward=feedforward_dim,
                                                    nhead=snapshot_num_heads)
        self.snapshot_encoder = nn.TransformerEncoder(snapshot_layer, num_layers=snapshot_num_layers)
        choice_layer = nn.TransformerEncoderLayer(d_model=embed_dim, dim_feedforward=feedforward_dim,
                                                  nhead=choice_num_heads)
        self.choice_encoder = nn.TransformerEncoder(choice_layer, num_layers=choice_num_layers)

    def encode(self, ids, mask, encoder, is_choices=False):
        if is_choices:
            batch_size, num_choices, seq_len = ids.size()
            ids = ids.view(batch_size * num_choices, -1)
            mask = mask.view(batch_size * num_choices, -1)
        embed = self.embedding(ids) * math.sqrt(self.embed_dim)
        embed = self.positional_encoding(embed)
        # pt transformer input must be of shape (seq_len, batch_size, ..) - inelegant
        embed = encoder(embed.transpose(1, 0), src_key_padding_mask=mask).transpose(0, 1)
        if is_choices:
            return embed.view(batch_size, num_choices, seq_len, -1)
        return embed

    def forward(self, snapshot_ids, snapshot_pad_mask, choices_ids, choices_pad_mask):
        snapshot_embed = self.encode(snapshot_ids, snapshot_pad_mask, self.snapshot_encoder)
        choices_embed = self.encode(choices_ids, choices_pad_mask, self.choice_encoder, is_choices=True)
        return snapshot_embed, choices_embed



