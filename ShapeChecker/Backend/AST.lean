import ShapeChecker.Backend.Basic

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
  | for : Name -> Expr -> List Stmt -> Stmt -> Stmt -- Require that the statement list be nonempty.

-- Consider functions to have a canonical form with one return only.
structure FunDef :=
  name : Name
  params : List (Name × Typ)
  body : List Stmt
  retval : Expr
  rettyp : Typ

def Program := List FunDef

def Py.Typ.toString : Typ → String
  | .int => "Int"
  | .float => "Float"
  | .list t => s!"List({t.toString})"
  | .tensor annot => s!"Tensor({annot})"

instance : ToString Py.Typ := ⟨Py.Typ.toString⟩

def Py.FunDef.toString : Py.FunDef → String
  | .funDef name body inputAnn outputAnn =>
    s!"function {name}() {\n  return {body.toString}\n}\nInput Types: {inputAnn}\nOutput Type: {outputAnn}"
