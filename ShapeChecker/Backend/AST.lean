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

inductive StmtIn :=
  | assign : Name -> Expr -> StmtIn
  | for : Name -> Expr -> List StmtIn -> StmtIn -> StmtIn -- Require that the statement list be nonempty.

-- TODO: Add some more statements
inductive Stmt :=
  | assign : Name -> Expr -> Stmt
  | for : Name -> Expr -> List Stmt -> Stmt -> Stmt -- Require that the statement list be nonempty.

structure FunDefIn :=
  name : Name
  params : List (Name × Typ)
  body : List StmtIn
  retval : Expr
  rettyp : Typ

structure FunDef :=
  name : Name
  params : List (Name × Typ)
  body : List Stmt
  retval : Expr
  rettyp : Typ

instance : Inhabited Stmt := ⟨.declare .anonymous $ .int 0⟩

partial def FunDefIn.toFunDef (f : FunDefIn) : FunDef :=
  { f with body := disambiguate f.body [] }
where disambiguate (s : List StmtIn) (Γ : List Name) :=
  match s with
  | [] => []
  | .assign n e ::xs =>
    if Γ.contains n then .mutate n e :: disambiguate xs Γ
    else .declare n e :: disambiguate xs (n :: Γ)
  | .for n e l s ::xs =>
    let re := disambiguate (l ++ [s]) Γ
    let x := re.take $ re.length - 1
    let y := (re.drop $ re.length - 1) |>.get! 0
    .for n e x y :: disambiguate xs Γ

-- Consider functions to have a canonical form with one return only.


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
