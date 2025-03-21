from torch import Tensor
import torch

def tensor_matmul(
    a: 'Tensor (l + [3, 3])',  # First matrix with batch dimensions l and matrix dims [a,b]
    b: 'Tensor (l + [3, 4])'   # Second matrix with matching batch dims l and matrix dims [b,c]
) -> 'Tensor (l + [3, 4])':    # Result has same batch dims l and matrix dims [a,c]
    return a @ b

# Example usage (not processed by shape checker, just for demonstration)
if __name__ == "__main__":
    # Create sample tensors with batch dim [2] and matrix dims [3,4] and [4,5]
    a = torch.randn(2, 3, 4)  # Shape: [2, 3, 4]
    b = torch.randn(2, 4, 5)  # Shape: [2, 4, 5]
    c = tensor_matmul(a, b)   # Shape: [2, 3, 5]
    print(f"Input shapes: {a.shape}, {b.shape}")
    print(f"Output shape: {c.shape}") 