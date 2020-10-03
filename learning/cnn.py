import torch
import torch.nn as nn

class GridCNN(nn.Module):
  def __init__(self, d_in, d_out, kernel_size):
    super(GridCNN, self).__init__()
    self.d_in = d_in
    self.d_out = d_out
    self.layers = nn.Sequential(nn.Conv2d(d_in, d_out, kernel_size=kernel_size),
                                nn.Conv2d(d_out,d_out,kernel_size=1),
                                nn.LeakyReLU(),
                                nn.Conv2d(d_out,d_out,kernel_size=1),
                                nn.LeakyReLU(),
                                nn.Conv2d(d_out,d_out,kernel_size=kernel_size))

  def forward(self, x):
    return self.layers(x)
