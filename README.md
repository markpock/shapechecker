# shapechecker

This project explores a novel approach to static shape checking of tensor operations in PyTorch programs. By transpiling PyTorch code into the dependently typed language Lean 4, we leverage its typechecker to verify shape correctness at compile-time. This project aims to eliminate common runtime tensor shape errors, enhancing debugging and developer productivity for machine learning and scientific computing.


### End to End Example of Our Pipeline 


```
def m(a: Tensor(3,4), b: Tensor(4,5)) -> Tensor(3,5):
    return a @ b
```

This is converted to an AST
```
Function(
    name = m
    arguments = [
        (name = a,
        annotator = 'Tensor(3,4)',
        positional = True),
        ...
    ], 
    body = ('RETURN, expression = (variable a) @ (variable b)) 
)
```

This AST is then accessed in C: 

Goal: Creating typed objects to represent this AST so that it could be read in to Lean as an inductive types (which we need to represent in C as Tagged Expressions). 

Read into C
```c
PyObject *p -> contains body, arguments, name 

```

Opt

Create TaggedExpr
```c
typedef struct taggedUExpr {
  int tag;
  union uExpr {
    int i;
    char* var;
    struct taggedUExpr* neg;
    struct eAdd {
      struct taggedUExpr* left;
      struct taggedUExpr* right;
    } a;
  } u;
} TaggedExpr;
```


Create traverse_expr turns taggedUExpr to a Lean object.


exprToC: (PyObject *value)



What is the ultimate output that we want? 
= A Lean Object given the values we define in Python 
















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


```
lake exe ShapeCheckerExe
<!-- compile the Lean code in your project into an executable format. This allows you to run Lean programs directly from the command line. -->
```

~TBD


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

