from torch import Tensor
import torch

# Tensor [..., a]
# Tensor [a, ...]
# Tensor [..., a, ...] -- should be illegal

def tensor_add(
    a : 'Tensor (l + [a, b])', 
    b : 'Tensor (l + [b, c])'  
) -> 'Tensor (l + [a, c])' :    
    return a + b

print(tensor_add(torch.zeros((1, 2, 3)), torch.zeros((1, 2, 3))))
