import ast
from typing import List, Dict, Any, Optional, Tuple

print("Loading script.py")

# Define expression types to match Lean types
class Expr:
    pass

class IntExpr(Expr):
    def __init__(self, value: int):
        self.tag = 0  # EXPR_INT
        self.value = value

class VarExpr(Expr):
    def __init__(self, name: str):
        self.tag = 1  # EXPR_VAR
        self.name = name

class NegExpr(Expr):
    def __init__(self, arg: Expr):
        self.tag = 2  # EXPR_NEG
        self.arg = arg

class AddExpr(Expr):
    def __init__(self, left: Expr, right: Expr):
        self.tag = 3  # EXPR_ADD
        self.left = left
        self.right = right

class MatMultExpr(Expr):
    def __init__(self, left: Expr, right: Expr):
        self.tag = 4  # EXPR_MATMULT
        self.left = left
        self.right = right

class TensorExpr(Expr):
    def __init__(self, dim1: Expr, dim2: Expr):
        self.tag = 5  # EXPR_TENSOR
        self.dim1 = dim1
        self.dim2 = dim2

# Define statement types to match Lean types
class Stmt:
    pass

class AssignStmt(Stmt):
    def __init__(self, name: str, value: Expr):
        self.tag = 0  # STMT_ASSIGN
        self.name = name
        self.value = value

class ReturnStmt(Stmt):
    def __init__(self, value: Expr):
        self.tag = 1  # STMT_RETURN
        self.value = value

# Function definition with all information needed by Lean
class FunctionDef:
    def __init__(self, name: str, params: List[Tuple[str, Expr]], return_type: Expr, body: List[Stmt], annotations: Dict[str, str]):
        self.name = name
        self.params = params  # List of (name, type) tuples
        self.param_count = len(params)
        self.return_type = return_type
        self.body = body
        self.body_count = len(body)
        self.annotations = annotations

# AST parsing functions
def parse_expr(node) -> Optional[Expr]:
    """Parse an AST node to our expression structure"""
    if node is None:
        return None
    
    # Handle constants (replaces ast.Num)
    if isinstance(node, ast.Constant) and isinstance(node.value, (int, float)):
        return IntExpr(int(node.value))
    
    elif isinstance(node, ast.Name):
        return VarExpr(node.id)
    
    elif isinstance(node, ast.UnaryOp) and isinstance(node.op, ast.USub):
        arg = parse_expr(node.operand)
        if arg:
            return NegExpr(arg)
    
    elif isinstance(node, ast.BinOp):
        left = parse_expr(node.left)
        right = parse_expr(node.right)
        
        if left and right:
            if isinstance(node.op, ast.Add):
                return AddExpr(left, right)
            elif isinstance(node.op, ast.MatMult):
                return MatMultExpr(left, right)
    
    elif isinstance(node, ast.Call) and isinstance(node.func, ast.Name) and node.func.id == 'Tensor':
        if len(node.args) == 2:
            dim1 = parse_expr(node.args[0])
            dim2 = parse_expr(node.args[1])
            if dim1 and dim2:
                return TensorExpr(dim1, dim2)
    
    # Handle string literals that might be tensor annotations
    elif isinstance(node, ast.Constant) and isinstance(node.value, str):
        # Try to parse strings like 'Tensor(3,4)' or 'Tensor (3, 4)'
        value = node.value.strip()
        if value.startswith('Tensor'):
            # Extract dimensions from the format "Tensor(dim1,dim2)" or "Tensor (dim1, dim2)"
            value = value.replace('Tensor', '').strip()
            if value.startswith('(') and value.endswith(')'):
                value = value[1:-1]  # Remove parentheses
                dims = [d.strip() for d in value.split(',')]
                if len(dims) == 2:
                    try:
                        dim1 = IntExpr(int(dims[0]))
                        dim2 = IntExpr(int(dims[1]))
                        return TensorExpr(dim1, dim2)
                    except ValueError:
                        pass
    
    return None

def parse_stmt(node) -> Optional[Stmt]:
    """Parse an AST statement node to our statement structure"""
    if isinstance(node, ast.Assign):
        if len(node.targets) == 1 and isinstance(node.targets[0], ast.Name):
            name = node.targets[0].id
            value = parse_expr(node.value)
            if value:
                return AssignStmt(name, value)
    
    elif isinstance(node, ast.Return):
        value = parse_expr(node.value)
        if value:
            return ReturnStmt(value)
    
    return None

def parse_annotation(node) -> Optional[Expr]:
    """Parse an annotation node to our expression structure"""
    if isinstance(node, ast.Constant) and isinstance(node.value, str):
        # Try to parse strings like 'Tensor(3,4)' or 'Tensor (3, 4)'
        value = node.value.strip()
        if value.startswith('Tensor'):
            # Extract dimensions from the format "Tensor(dim1,dim2)" or "Tensor (dim1, dim2)"
            value = value.replace('Tensor', '').strip()
            if value.startswith('(') and value.endswith(')'):
                value = value[1:-1]  # Remove parentheses
                dims = [d.strip() for d in value.split(',')]
                if len(dims) == 2:
                    try:
                        dim1 = IntExpr(int(dims[0]))
                        dim2 = IntExpr(int(dims[1]))
                        return TensorExpr(dim1, dim2)
                    except ValueError:
                        pass
    return None

def parse_function_def(source: str) -> Optional[FunctionDef]:
    """Parse a function definition from source code string"""
    try:
        module = ast.parse(source)
        if not module.body or not isinstance(module.body[0], ast.FunctionDef):
            return None
        
        func_def = module.body[0]
        name = func_def.name
        
        # Parse parameters and their types
        params = []
        for arg in func_def.args.args:
            arg_name = arg.arg
            arg_type = parse_annotation(arg.annotation)
            if arg_type:
                params.append((arg_name, arg_type))
        
        # Parse return type
        return_type = parse_annotation(func_def.returns)
        
        # Parse function body
        body = []
        for stmt in func_def.body:
            parsed_stmt = parse_stmt(stmt)
            if parsed_stmt:
                body.append(parsed_stmt)
        
        # Extract annotations for metadata
        annotations = {}
        for i, arg in enumerate(func_def.args.args):
            if arg.annotation and isinstance(arg.annotation, ast.Constant):
                arg_name = arg.arg
                annotations[f"input_{arg_name}"] = arg.annotation.value
        
        if func_def.returns and isinstance(func_def.returns, ast.Constant):
            annotations["output"] = func_def.returns.value
        
        annotations["function_name"] = name
        
        return FunctionDef(name, params, return_type, body, annotations)
    
    except Exception as e:
        print(f"Error parsing function: {e}")
        return None

def make_function():
    """Create a matrix multiplication function with tensor annotations and an assignment"""
    source = """
def matrix_multiply(a: 'Tensor (8, 4)', b: 'Tensor (4, 5)') -> 'Tensor (8, 5)':
    c = a
    d = b
    return a @ b
"""
    return parse_function_def(source)

def extract_annotations_from_ast() -> Dict[str, str]:
    """Extract tensor annotations from the function AST"""
    func_def = make_function()
    if not func_def:
        return {}
    
    return func_def.annotations

# This function can be called from C to get the annotations directly
def get_annotations_as_dict():
    """Return tensor annotations as a dictionary"""
    return extract_annotations_from_ast()

# For C to access the parsed function
def get_parsed_function():
    """Return the fully parsed function for C code to access"""
    return make_function()

if __name__ == "__main__":
    # Current directory for debugging
    import os
    print(os.getcwd())
    
    # Test the function extraction
    func = make_function()
    if func:
        print(f"\nParsed Function: {func.name}")
        print(f"Parameters: {[(p[0], 'Tensor' if isinstance(p[1], TensorExpr) else 'Other') for p in func.params]}")
        print(f"Return Type: {'Tensor' if isinstance(func.return_type, TensorExpr) else 'Other'}")
        print("Body:")
        for stmt in func.body:
            if isinstance(stmt, AssignStmt):
                print(f"  Assignment: {stmt.name} = (expression)")
            elif isinstance(stmt, ReturnStmt):
                print("  Return: (expression)")