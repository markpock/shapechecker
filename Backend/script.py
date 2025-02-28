class Tree:
    def __init__(self, data: int, left : 'Tree' = None, right : 'Tree' = None):
        self.left = left
        self.right = right
        self.data = data

# script.py
def greet():
    return Tree(3, Tree(2), Tree(1))