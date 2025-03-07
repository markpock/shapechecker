'''
# In Python (Code -> AST)
def tensor_add(
    a : 'Tensor (l + [a, b])', 
    b : 'Tensor (l + [a, b])'
) -> 'Tensor (l + [a, b])':    
    return a + b

# In Lean (AST)
def tensor_add (a : Tensor 'a ...) (b : Tensor) : Tensor (l + [a, b]) :=
  a + b

'''
import ast
import json

def translate_operator(op):
    if isinstance(op, ast.Add):
        return "+"
    elif isinstance(op, ast.Sub):
        return "-"
    elif isinstance(op, ast.Mult):
        return "*"
    elif isinstance(op, ast.Div):
        return "/"
    return "?"

class PythonToLeanTranslator(ast.NodeVisitor):
    def __init__(self):
        self.lean_lines = []
    
    def translate(self, node):
        self.visit(node)
        return "\n".join(self.lean_lines)
    
    def visit_Module(self, node: ast.Module):
        for stmt in node.body:
            self.visit(stmt)
    
    def visit_FunctionDef(self, node: ast.FunctionDef):
        func_name = node.name
        arg_list = []
        for arg in node.args.args:
            if arg.annotation:
                arg_annotation = self.visit(arg.annotation)
            else:
                arg_annotation = "ℕ"
            arg_list.append(f"{arg.arg} : {arg_annotation}")
        args_str = " ".join(arg_list)
        
        # Process the return annotation if present
        ret_type = self.visit(node.returns) if node.returns else "ℕ"
        
        # For simplicity, assume function body is a single return statement.
        body_expr = ""
        for stmt in node.body:
            if isinstance(stmt, ast.Return):
                body_expr = self.visit(stmt.value)
            else:
                body_expr += self.visit(stmt)
        
        # Construct the Lean function definition.
        lean_def = f"def {func_name} ({args_str}) : {ret_type} :=\n  {body_expr}"
        self.lean_lines.append(lean_def)
        self.lean_lines.append("")  # Blank line for separation
    
    def visit_Return(self, node: ast.Return):
        return self.visit(node.value)
    
    def visit_BinOp(self, node: ast.BinOp):
        left = self.visit(node.left)
        right = self.visit(node.right)
        op = translate_operator(node.op)
        return f"{left} {op} {right}"
    
    def visit_Name(self, node: ast.Name):
        return node.id
    
    def visit_Constant(self, node: ast.Constant):
        if isinstance(node.value, str):
            # Handle string constants, especially for type annotations
            return node.value
        return repr(node.value)
    
    def visit_Expr(self, node: ast.Expr):
        return self.visit(node.value)
    
    def visit_Call(self, node: ast.Call):
        func = self.visit(node.func)
        args = " ".join(self.visit(arg) for arg in node.args)
        return f"{func} {args}"
    
    def visit_Attribute(self, node: ast.Attribute):
        value = self.visit(node.value)
        return f"{value}.{node.attr}"
    
    def visit_Subscript(self, node: ast.Subscript):
        # Handle subscript notation like Tensor['a', ...]
        value = self.visit(node.value)
        slice_value = self.visit(node.slice)
        return f"{value} {slice_value}"
    
    def visit_Index(self, node: ast.Index):
        # For Python < 3.9
        return self.visit(node.value)
    
    def visit_Tuple(self, node: ast.Tuple):
        elts = [self.visit(elt) for elt in node.elts]
        return " ".join(elts)
    
    def visit_Constant(self, node: ast.Constant):
        if isinstance(node.value, str):
            # Special case for tensor type annotations
            if "Tensor" in node.value:
                # Extract the tensor parameters from the string
                return node.value.replace("'", "")
            return f"'{node.value}'"
        return str(node.value)
    
    def visit_Ellipsis(self, node: ast.Ellipsis):
        return "..."
    
    def visit_List(self, node: ast.List):
        elts = [self.visit(elt) for elt in node.elts]
        return f"[{', '.join(elts)}]"
    
    def visit_BinOp(self, node: ast.BinOp):
        left = self.visit(node.left)
        right = self.visit(node.right)
        op = translate_operator(node.op)
        return f"{left} {op} {right}"
    
    def visit_JoinedStr(self, node: ast.JoinedStr):
        # Handle f-strings
        values = []
        for value in node.values:
            values.append(self.visit(value))
        return "".join(values)
    
    def visit_FormattedValue(self, node: ast.FormattedValue):
        # Part of f-string handling
        return "{" + self.visit(node.value) + "}"
    
    def visit_Starred(self, node: ast.Starred):
        # Handle starred expressions like *args
        return f"*{self.visit(node.value)}"
    
    def generic_visit(self, node):
        # Fallback: for nodes we haven't explicitly handled.
        return f"<{node.__class__.__name__}>"


def ast_to_dict(node) -> dict:
    """
    Converts a Python AST to a clean dictionary structure with only the essential 
    information needed for Lean translation.
    
    Args:
        node: An AST node
        
    Returns:
        A dictionary representation with just the needed information
    """
    if node is None:
        return None
    
    if isinstance(node, ast.Module):
        # Only process the first function definition in the module
        for stmt in node.body:
            if isinstance(stmt, ast.FunctionDef):
                return ast_to_dict(stmt)
        return {}
    
    elif isinstance(node, ast.FunctionDef):
        # Extract function information
        result = {
            "name": node.name,
            "args": [],
            "body": {}
        }
        
        # Process arguments
        for arg in node.args.args:
            arg_info = {"name": arg.arg}
            if arg.annotation and isinstance(arg.annotation, ast.Constant):
                arg_info["type"] = arg.annotation.value
            result["args"].append(arg_info)
        
        # Process return type if present
        if node.returns and isinstance(node.returns, ast.Constant):
            result["return_type"] = node.returns.value
        
        # Process function body (assume it's a return statement)
        for stmt in node.body:
            if isinstance(stmt, ast.Return):
                result["body"] = ast_to_dict(stmt.value)
        
        return result
    
    elif isinstance(node, ast.BinOp):
        # Process binary operation
        op_map = {
            ast.Add: "+",
            ast.Sub: "-",
            ast.Mult: "*",
            ast.Div: "/"
        }
        
        op_symbol = "?"
        if isinstance(node.op, tuple(op_map.keys())):
            op_symbol = op_map[type(node.op)]
        
        return {
            "operation": op_symbol,
            "left": ast_to_dict(node.left),
            "right": ast_to_dict(node.right)
        }
    
    elif isinstance(node, ast.Name):
        # Process variable names
        return {"variable": node.id}
    
    # Fallback for any other nodes
    return {"unhandled": node.__class__.__name__}

def prune_and_convert(python_code):
    """
    Parse Python code, prune the AST, and convert to dictionary format
    
    Args:
        python_code: Python code as a string
        
    Returns:
        Dictionary representation of the pruned AST
    """
    tree = ast.parse(python_code)
    ast_dict = ast_to_dict(tree)
    return ast_dict

def test_conversion():
    """Test the AST to dictionary conversion with an example"""
    code = """
def tensor_add(
    a : 'Tensor (l + [a, b])', 
    b : 'Tensor (l + [a, b])'
) -> 'Tensor (l + [a, b])':    
    return a + b
"""
    result = prune_and_convert(code)
    print(type(result))
    print(json.dumps(result, indent=2))

if __name__ == "__main__":
    test_conversion()
