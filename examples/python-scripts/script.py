class Tree:
    def __init__(self, data: int, left : 'Tree' = None, right : 'Tree' = None):
        self.left = left
        self.right = right
        self.data = data


class Expr:
    def __init__(self, integer: int = 0, _float: float = 0.0, neg: 'Expr' = None, var: str = ""):
        self.integer = integer
        self.float = _float
        self.neg = neg
        self.var = var

    # Add factory methods to create different expression types
    @classmethod
    def create_int(cls, value: int):
        return cls(integer=value)
    
    @classmethod
    def create_float(cls, value: float):
        return cls(_float=value)
    
    @classmethod
    def create_var(cls, name: str):
        return cls(var=name)
    
    @classmethod
    def create_neg(cls, expr):
        return cls(neg=expr)

# script.py
def greet():
    return Tree(3, Tree(2), Tree(1))

def greet2():
    # Create an integer expression with value 42
    return Expr.create_int(42)

# Additional test functions for other expression types
def make_float_expr():
    return Expr.create_float(3.14)

def make_var_expr():
    return Expr.create_var("x")

def make_neg_expr():
    # Create a negation of integer 5
    return Expr.create_neg(Expr.create_int(5))

def make_complex_expr():
    # Create a variable expression "y"
    var_y = Expr.create_var("y")
    # Create negation of y
    return Expr.create_neg(var_y)