
/-
```python

"(1 + 2) + (3 + 4)" --> parse this into the tree below

(1 + 2) + 3


     +
   /   \
  +      +
 / \    / \
1  2   3   4

abstract class Tree {}
class Empty extends Tree {}
class Plus extends Tree {
  Tree left, right;
}

     +
   /   \
  +     3
 / \
1  2

class Tree
class Leaf(Tree)
  n : int

@dataclass class Plus(Tree)
  left : Tree
  right : Tree


Plus(Plus(Leaf(1), Leaf(2)), Leaf(3))





"(1 + 1) * 3" --> parse it into some structured object
     *
   /   \
  +     3
 / \
1  1

class Tree(abc):
  def calc(self): ...

class Leaf(Tree):
  n : int

  def calc(self):
    return self.n

class Plus(Tree):
  left : Tree
  right : Tree
  def __init__(self, left : Tree, right : Tree): ...

  def calc(self):
    return self.left.calc() + self.right.calc()

class Mul(Tree):
  left : Tree
  right : Tree

  def calc(self):
    return self.left.calc() * self.right.calc()


     +
   /   \
  *     3
 / \
1  1

Plus(Mul(Leaf(1), Leaf(2)), Leaf(3)).calc()


abstract class Arith {
  ...
}

class Const extends Arith {
  int n;
  ...
}

class Add extends Arith {
  Arith e1, e2;
  ...

}

class Mult extends Arith {
  Arith e1, e2;
  ...

}
```

-/




/-
def f ():
  x = 3
  y = 2 + x
  return y + x

def g (): return 5

-/

inductive Expression :=
  | const (n : Nat)
  | var (s : String)
  | plus   : Expression -> Expression -> Expression
  | mul    : Expression -> Expression -> Expression

inductive PrimOp :=
  | shape
  | reshape
  | stack
  | matmul
  | add
  | mean
  | softmax
  | zeros
  | ones
  | tensor
  | slicing

inductive Statement :=
  | assign (name : String) (expression : Expression)
  | ret (expression : Expression)

def Function := List Statement
def Program := List Function

example : Function := [
  .assign "x" (.const 3),
  .assign "y" (.plus (.const 2) (  ))
]

variable (Tensor : List Nat -> Type)

example (a b : Tensor [1, 2, 3]) : Function (Tensor [1, 2, 3]) := [
  .assign "x" (a + b),
  ret (.var "x")
]

inductive Arith' :=
  | Leaf (n : Nat)
  | Plus : Arith' -> (Arith' -> Arith')
  | Mul : Arith' -> (Arith' -> Arith')

def add1 : Nat × Nat -> Nat := fun (a, b) => a + b
def add2 : Nat -> (Nat -> Nat) := fun a => fun b => a + b
/-

def f (a) :
  def g (b) :
    return a + b
  return g

f (3)
=
def g (b) :
  return 3 + b
return g


add2 (3)
  ~> ( fun a => fun b => a + b ) (3)
  ~> ( fun b => 3 + b )
add2 (3) (4)
  ~> ( (fun a => fun b => a + b) (3) ) (4)
  ~> ( fun b => 3 + b ) (4)
  3 + 4

-/

#reduce ((fun a => fun b => a + b) 3)


#check ((3, 2) : Nat × Nat)

def Arith.calc (self : Arith) : Nat :=
  match self with
  | Arith.Leaf n => n
  | Arith.Plus (left, right) => left.calc + right.calc
  | Arith.Mul  (left, right) => left.calc * right.calc

#check (.Plus (.Mul (.Leaf 1, .Leaf 2), .Leaf 3) : Arith)
#check (.Plus (.Mul (.Leaf 1, .Leaf 2), .Leaf 3) |> Arith.calc)
