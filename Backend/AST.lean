import Backend.Basic
import Lean
open Lean

namespace Py
inductive Typ :=
  | int
  | float
  | list (t : Typ)
  | tensor (annot : Shape)

-- TODO: Add some more operators
inductive Expr :=
  -- Constants
  | int (n : Int)
  | float (f : Float)

  | list (l : List Expr)

  -- The tensor constants
  | zeros (s : Shape)
  | ones (s : Shape)
  | random (s : Shape)

  | neg (arg : Expr)
  -- The basic binary operations
  | add (left right : Expr)
  | sub (left right : Expr)
  | mul (left right : Expr)
  | div (left right : Expr)
  | pow (left right : Expr)
  | matmul (left right : Expr)

  | var (v : Name)
  | call (fn : Expr) (args : List Expr)

-- TODO: Add some more statements
inductive Stmt :=
  | assign : Name -> Expr -> Stmt

-- Consider functions to have a canonical form with one return only.
structure FunDef :=
  name : Name
  params : List (Name × Typ)
  body : List Stmt
  retval : Expr
  rettyp : Typ

def Program := List FunDef

end Py
