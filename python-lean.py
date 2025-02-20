import ast

def translate_operator(op):
    match op: 
        case isinstance(op, ast.Add):
            return "+"
        case isinstance(op, ast.Sub):
            return "-"
        case isinstance(op, ast.Mult):
            return "*"
        case isinstance(op, ast.Div):
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
                body_expr += self.generic_visit(stmt)
        
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
        return f"({left} {op} {right})"
    
    def visit_Name(self, node: ast.Name):
        return node.id
    
    def visit_Constant(self, node: ast.Constant):
        return repr(node.value)
    
    # For Python versions < 3.8, strings may come as ast.Str
    def visit_Str(self, node: ast.Str):
        return node.s
    
    def visit_Expr(self, node: ast.Expr):
        return self.visit(node.value)
    
    def visit_Call(self, node: ast.Call):
        func = self.visit(node.func)
        args = " ".join(self.visit(arg) for arg in node.args)
        return f"{func} {args}"
    
    def visit_Attribute(self, node: ast.Attribute):
        value = self.visit(node.value)
        return f"{value}.{node.attr}"
    
    def generic_visit(self, node):
        # Fallback: for nodes we haven't explicitly handled.
        return f"<{node.__class__.__name__}>"

def translate_code_to_lean(python_code: str) -> str:
    tree = ast.parse(python_code)
    
    translator = PythonToLeanTranslator()
    return translator.translate(tree)

if __name__ == "__main__":
    code = """
def tensor_add(
    a : Tensor['a', ...], 
    b : Tensor[]
) -> 'Tensor (l + [a, b])':    
    return a + b

print(tensor_add(torch.zeros((1, 2, 3)), torch.zeros((1, 2, 3))))
    """
    lean_output = translate_code_to_lean(code)
    print("Translated Lean Code:")
    print(lean_output)