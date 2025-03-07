class Tree:
    data : int
    left : 'Tree'
    right : 'Tree'

    def __init__(self, data : int, left : 'Tree' = None, right : 'Tree' = None):
        self.data = data
        self.left = left
        self.right = right

def simpleTree():
    return Tree(1, Tree(2, Tree(4)), Tree(3))
