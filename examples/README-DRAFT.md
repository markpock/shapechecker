
### End to End Example of Our Pipeline 

Take this annotation done by the deep learning programmer in PyTorch: 

```python
def m(a: Tensor(3,4), b: Tensor(4,5)) -> Tensor(3,5):
    return a @ b
```

**This is converted to an AST**:
```
Module(
    body=[
        FunctionDef(
            name='m',
            args=arguments(
                args=[
                    arg(arg='a', annotation=Call(func=Name(id='Tensor', ctx=Load()), args=[Constant(value=3), Constant(value=4)], keywords=[])),
                    arg(arg='b', annotation=Call(func=Name(id='Tensor', ctx=Load()), args=[Constant(value=4), Constant(value=5)], keywords=[]))
                ],
                vararg=None,
                kwonlyargs=[],
                kw_defaults=[],
                kwarg=None,
                defaults=[]
            ),
            body=[
                Return(
                    value=BinOp(left=Name(id='a', ctx=Load()), op=MatMult(), right=Name(id='b', ctx=Load()))
                )
            ],
            decorator_list=[]
        )
    ]
)
```

**This AST is then accessed in C:**

Goal: Creating typed objects to represent this AST so that it could be read in to Lean as an inductive types (which we need to represent in C as Tagged Expressions). 

Read into C
```c
PyObject *p -> contains body, arguments, name 
```

Create TaggedExpr

```c
// Structure to represent a function
typedef struct Function {
    char* name;                 // Function name
    struct Parameter* params;   // Array of parameters
    int param_count;            // Number of parameters
    struct TaggedExpr* return_type; // Return type
    struct TaggedExpr* body;    // Function body
} Function;

// Structure to represent a parameter
typedef struct Parameter {
    char* name;                 // Parameter name
    struct TaggedExpr* type;    // Parameter type
} Parameter;

// Create the function 'm'
Function* create_function_m() {
    Function* func = (Function*)malloc(sizeof(Function));
    func->name = strdup("m");
    
    // Create parameters
    func->param_count = 2;
    func->params = (Parameter*)malloc(sizeof(Parameter) * func->param_count);
    
    // Parameter 'a: Tensor(3,4)'
    func->params[0].name = strdup("a");
    TaggedExpr* tensor_3_4 = createTensorType(createConstant(3), createConstant(4));
    func->params[0].type = tensor_3_4;
    
    // Parameter 'b: Tensor(4,5)'
    func->params[1].name = strdup("b");
    TaggedExpr* tensor_4_5 = createTensorType(createConstant(4), createConstant(5));
    func->params[1].type = tensor_4_5;
    
    // Return type 'Tensor(3,5)'
    func->return_type = createTensorType(createConstant(3), createConstant(5));
    
    // Function body 'return a @ b'
    TaggedExpr* var_a = createVariable("a");
    TaggedExpr* var_b = createVariable("b");
    TaggedExpr* matmul = createMatrixMult(var_a, var_b);
    func->body = createReturn(matmul);
    
    return func;
}
```

Create traverse_expr turns taggedUExpr to a Lean object.


exprToC: (PyObject *value)


What is the ultimate output that we want? 
= A Lean Object given the values we define in Python 


To run this:
```sh
lake exe ShapeCheckerExe
```





---






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

~TBD


### Roadmap
1.	MVP: Develop a working prototype that transpiles simple PyTorch tensor operations to Lean 4 and verifies shape correctness.
2.	Program Slicing: Implement slicing to isolate shape-relevant control flow from complex Python code.
3.	Descriptive and actionable error messages for developers.
4.	GUI Tooling: For increased usability. 
5.	Benchmarking: Test on real-world PyTorch programs from GitHub to evaluate effectiveness and performance.
