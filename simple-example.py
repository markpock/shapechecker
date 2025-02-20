from torch import Tensor
import torch

# Tensor [..., a]
# Tensor [a, ...]
# Tensor [..., a, ...] -- should be illegal

def tensor_matmul(
    a : 'Tensor (l + [a, b])', # Tensor (l ++ [a, b])    Tensor [..., a, b]
    b : 'Tensor (l + [b, c])'  # Tensor (l ++ [b, c])    Tensor [..., b, c]
) -> 'Tensor (l + [a, c])' :   # Tensor (l ++ [a, c])
    return a @ b

# Works
print(tensor_matmul(torch.zeros((2, 3)), torch.zeros((3, 4))))
# Works
print(tensor_matmul(torch.zeros((5, 2, 3)), torch.zeros((5, 3, 4))))
# Does not work
print(tensor_matmul(torch.zeros((5, 2, 3)), torch.zeros((4, 3, 4))))
# Also does not work
print(tensor_matmul(torch.zeros((5, 2, 3)), torch.zeros((5, 2, 4))))
# Does work... but don't worry about this for now.....
print(tensor_matmul(torch.zeros((5, 2, 3)), torch.zeros((3, 4))))