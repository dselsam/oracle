#  Can later be changed to argparse
from dataclasses import dataclass


@dataclass
class TextEncoderConfig:
    embed_dim: int = 64
    snapshot_num_heads: int = 4
    snapshot_num_layers: int = 1
    choice_num_heads: int = 4
    choice_num_layers: int = 1
    feedforward_dim: int = 512
    dropout: float = 0.1


@dataclass
class TrainConfig:
    learning_rate: float = 1e-4
    n_embed_mlp_layers: int = 2
    n_reasoner_layers: int = 3
