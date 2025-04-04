# shapechecker

This project explores a novel approach to static shape checking of tensor operations in PyTorch programs. By compiling PyTorch code into the dependently typed language Lean 4, we leverage its typechecker and tactics to verify shape correctness at compile-time. This project aims to eliminate common runtime tensor shape errors which make up 40% of all deep learning bugs, enhancing debugging and developer productivity for machine learning and scientific computing.

Please check out our paper [here](/Users/matthewtaruno/Library/Mobile Documents/com~apple~CloudDocs/Dev/shapechecker/CheckedShapes.pdf) for an in-depth discussion. 

Special thanks to the Lean and PyTorch communities for their tools and resources.

