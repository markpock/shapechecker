class Tree:
    def __init__(self, data: int, left : 'Tree' = None, right : 'Tree' = None):
        self.left = left
        self.right = right
        self.data = data


class Expr:
    def __init__(self, integer: int = None, var: str = None, neg: 'Expr' = None, left: 'Expr' = None, right: 'Expr' = None):
        self.integer = integer
        self.var = var
        self.neg = neg
        self.left = left
        self.right = right

    @classmethod
    def create_int(cls, value: int):
        return cls(integer=value)
    
    @classmethod
    def create_var(cls, name: str):
        return cls(var=name)
    
    @classmethod
    def create_neg(cls, expr):
        return cls(neg=expr)
    
    @classmethod
    def create_add(cls, left, right):
        return cls(left=left, right=right)

# script.py
def greet():
    return Tree(3, Tree(2), Tree(1))

def make_int_expr():
    # Create an integer expression with value 42
    print("made the int_expr")
    return Expr.create_int(42)

def make_var_expr():
    print("made the var_Expr")
    return Expr.create_var("x")

def make_neg_expr():
    # Create a negation of integer 5
    return Expr.create_neg(Expr.create_int(5))

def make_add_expr():
    # Create an addition: 1 + 2
    return Expr.create_add(Expr.create_int(1), Expr.create_int(2))