import torch
import torch.nn as nn

def decode_activation(activation):
  if activation == "relu":
    return nn.ReLU
  elif activation == "leaky_relu":
    return nn.LeakyReLU
  elif activation == "relu6":
    return nn.ReLU6
  elif activation == "elu":
    return nn.ELU
  else:
    raise Exception("unsupported activation")

# joe average MLP
class BasicMLP(nn.Module):
  def __init__(self, input_dim, hidden_dims, output_dim, activation, bias_at_end=True, p_dropout=0.1, **kwargs):
    super(BasicMLP, self).__init__(**kwargs)
    layers = []
    for k in range(len(hidden_dims) + 1):
      if k == 0:
        d_in = input_dim
      else:
        d_in = hidden_dims[k-1]
      if k == len(hidden_dims):
        d_out = output_dim
      else:
        d_out = hidden_dims[k]
      layers.append(nn.Linear(in_features=d_in,
                              out_features=d_out,
                              bias=(True if ((k == len(hidden_dims) and bias_at_end) or k < len(hidden_dims)) else False)))
      if not (k == len(hidden_dims)):
        layers.append(decode_activation(activation)())
        layers.append(nn.Dropout(p_dropout))
    self.main = nn.Sequential(*layers)

  def forward(self, z):
    return self.main(z)
