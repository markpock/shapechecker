# shapechecker


This project explores a novel approach to static shape checking of tensor operations in PyTorch programs. By transpiling PyTorch code into the dependently typed language Lean 4, we leverage its typechecker to verify shape correctness at compile-time. This project aims to eliminate common runtime tensor shape errors, enhancing debugging and developer productivity for machine learning and scientific computing.


### Introduction

One of the most common classes of errors in machine learning programs is Tensor shape mismatches - an error which comes from operations on tensors of incompatible shape. Consider the matrix multiplication:

`np.array([[1, 2, 3]]) @ np.array([[1], [2]])`

As a multiplication of a 1 by 3 tensor and a 2 by 1 tensor, the operands have incompatible shape, and so the program produces the difficult-to-interpret runtime error:

`ValueError: matmul: Input operand 1 has a mismatch in its core dimension 0, with gufunc signature (n?,k),(k,m?)->(n?,m?) (size 2 is different from 3).`

Tensor shape mismatches are a frequent and frustrating source of bugs in Python's machine learning workflows. This error should not be difficult to diagnose, and a static analysis would almost certainly be able to catch this in a language with a stronger type system. However, static analysis in Python is made quite difficult by Python’s type system, and so even widely used Python type checkers like `mypy` are unable to collect the information required to report this error. 

This brings us to our implementation of a Shape Checker to overcome this. We provide a static analysis tool that:
1. Transpiles PyTorch code to Lean 4: Uses Lean 4's dependent type system to verify tensor shape correctness.
2. Identifies shape-related errors at compile-time: By encoding tensor dimensions into Lean 4's types, we prevent shape mismatches ahead fo runtime.
3. Simplifies debugging with readable errors: Generates clear and actionable shape error mesages for developers.

### Features
- Static shape checking
- Program slicing
- Python-to-Lean Transpilation
- Human-readable errors

### Set Up Lean 4:
Follow the instructions on the Lean 4 installation guide.


### Usage: 
TBD~


### Roadmap
1.	MVP: Develop a working prototype that transpiles simple PyTorch tensor operations to Lean 4 and verifies shape correctness.
2.	Program Slicing: Implement slicing to isolate shape-relevant control flow from complex Python code.
3.	Descriptive and actionable error messages for developers.
4.	GUI Tooling: For increased usability. 
5.	Benchmarking: Test on real-world PyTorch programs from GitHub to evaluate effectiveness and performance.


### Acknowledgments

This project draws inspiration from prior work, including:
- [ShapeChecker: ShapeChecker: Inferring and Reasoning about Shapes in TensorFlow]([url](https://courses.cs.washington.edu/courses/cse503/18wi/submissions/503-final-report-20180315/shape-checker.pdf))
- [Ezyang’s Blog: A Compile-Time Debugger for Tensor Shape Checks]([url](http://blog.ezyang.com/2018/04/a-compile-time-debugger-that-helps-you-write-tensor-shape-checks/))

Special thanks to the Lean and PyTorch communities for their tools and resources.
