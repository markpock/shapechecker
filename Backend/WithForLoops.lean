import Lean

open Lean

namespace Py

inductive ShapeData :=
  | wildcard
  | var (x : Name)
  | const (n : Nat)

inductive Shape :=
  | nil
  | wildcard
  | var (x : Name)
  | cons (data : ShapeData) (rest : Shape)
  | app (op1 op2 : Shape)

inductive Typ :=
  | int
  | float
  | list (t : Typ)
  | tensor (annot : Shape)

inductive Expr :=
  | int (n : Int)
  | float (f : Float)
  | neg (arg : Expr)
  | var (v : Name)
  | add (left right : Expr)
  | matmul (left right : Expr)
  | call (fn : Expr) (args : List Expr)

inductive Stmt :=
  | assign : Name -> Expr -> Stmt
  | forLoop : List Stmt -> Stmt

-- Consider functions to have a canonical form with one return only.
structure FunDef :=
  name : Name
  params : List (Name × Typ)
  body : List Stmt
  retval : Expr
  rettyp : Typ

def Program := List FunDef

end Py


namespace PyExample
def tN : String -> Name := (Name.str .anonymous ·)

#check (.app (.var $ tN "l") $ .cons (.var $ tN "a") $ .cons (.var $ tN "b") $ .nil : Py.Shape)
end PyExample

def Tensor (l : Py.Shape) := { ℓ : Py.Shape // l = ℓ }
namespace Tensor
def mk : Tensor l := ⟨l, by rfl⟩
def of (l : Py.Shape) : Tensor l := ⟨l, by rfl⟩
end Tensor


def isPrimOp (n : Name) : Bool :=
  match n with
  | .str .anonymous s =>
    s == "tensorAddPrimOp"
  | _ => false

open Elab Command Term Meta

namespace ElabPy

#check Lean.Parser.Term.doSeq
mutual
partial def expr (e : Py.Expr) : CommandElabM (TSyntax `term) := do
  match e with
  | .int n =>
    if n >= 0 then return quote (Int.toNat n)
    else `(- $(quote (Int.toNat (-n))))
  | .float n => return Syntax.mkScientificLit (toString n)
  | .neg arg => `((-$(<- expr arg)))
  | .var v => return mkIdent v
  | .add left right => `(($(<- expr left) + $(<- expr right)))
  | .matmul left right => `(($(<- expr left) @ $(<- expr right)))
  | .call (.var n) args =>
    if isPrimOp n then `($(<- fncall (.var n) args) ($(mkIdent `primOpResolve)))
    else fncall (.var n) args
  | .call fn args => fncall fn args

partial def fncall (fn : Py.Expr) (args : List Py.Expr) : CommandElabM (TSyntax `term) := do
  match args with
  | [] => expr fn
  | x::xs => `(($(<- fncall fn xs) $(<- expr x)))
end

def stmt (s : List Py.Stmt) (r : Py.Expr) : CommandElabM (TSyntax `Lean.Parser.Term.doSeq) := do
  match s with
  | [] => expr r
  | (.assign s ex)::xs =>`(
    let $(mkIdent s) := $(<- expr ex)
    $(<- stmt xs r)
  )

def unAnnot : Py.Shape -> CommandElabM (TSyntax `term) := λ s => do
  match s with
  | .nil => return mkIdent `Py.Shape.nil
  | .wildcard => return mkIdent `Py.Shape.wildcard
  | .var x => `($(mkIdent `Py.Shape.var) $(mkIdent x))
  | .cons .wildcard rest =>
    `($(mkIdent `Py.Shape.cons) $(mkIdent `Py.Shape.wildcard) $(<- unAnnot rest))
  | .cons (.var x) rest =>
    `($(mkIdent `Py.Shape.cons) ($(mkIdent `Py.Shape.var) $(mkIdent x)) $(<- unAnnot rest))
  | .cons (.const n) rest =>
    `($(mkIdent `Py.Shape.cons) ($(mkIdent `Py.Shape.const) $(quote n)) $(<- unAnnot rest))
  | .app op1 op2 => do
    `($(mkIdent `Py.Shape.app) $(<- unAnnot op1) $(<- unAnnot op2))

def typ : Py.Typ -> CommandElabM (TSyntax `term) := λ t => do
  match t with
  | .int => return mkIdent `Int
  | .float => return mkIdent `Float
  | .list t => `($(mkIdent `List) $(<- typ t))
  | .tensor annot => `($(mkIdent `Tensor) $(<- unAnnot annot))

def func : Py.FunDef -> CommandElabM (TSyntax `command) :=
  λ ⟨n, p, b, r, rt⟩ => do
  let params := if p.length > 0 then p.toArray.map Prod.fst |>.map mkIdent else #[mkIdent `_]

  let s := <- stmt b r
  `(def $(mkIdent n) : $(<- type (p.map Prod.snd) rt) := λ $(params)* => Id.run do
    $(s))
  where
    type (ps : List Py.Typ) (rt : Py.Typ) : CommandElabM (TSyntax `term) := do
      if ps.length == 0 then
        dbg_trace "Hello world"
        `($(mkIdent `Unit) -> $(<- typ rt))
      else typeNonEmpty ps rt
    typeNonEmpty (ps : List Py.Typ) (rt : Py.Typ) : CommandElabM (TSyntax `term) := do match ps with
      | [] => typ rt
      | x::xs => `($(<- typ x) -> $(<- typeNonEmpty xs rt))

end ElabPy

syntax (name := pyFuncCmd) "#pyFunc" term : command

@[command_elab pyFuncCmd] unsafe def elabPyFunc : CommandElab := λ s => match s with
  | `(command| #pyFunc $t:term) => do
    let pyfunc <- liftTermElabM $ elabTerm t none
    let cmd <- ElabPy.func (<- liftTermElabM $ evalExpr' Py.FunDef `Py.FunDef pyfunc)
    -- let f : Format <- liftCoreM $ PrettyPrinter.ppCommand cmd
    -- dbg_trace s!"{f}"
    elabCommand cmd
    liftTermElabM $ Tactic.TryThis.addSuggestion cmd.raw {suggestion := .tsyntax cmd}
  | _ => throwUnsupportedSyntax

/-
def exa(a : int, b : int) -> int:
  x = a
  y = x + 1
  return x
-/
def exampleFunction : Py.FunDef := {
  name := `exa
  params := [
    (`a, .int),
    (`b, .int)
  ]
  body := [
    .assign `x $ .var `a,
    .assign `y $ .add (.var `x) (.int 1)
  ]
  retval := .var `x
  rettyp := .int
}

/-
def tensorAddN(a : Tensor (l), b : Tensor (l)) -> Tensor (l):
  return a + b
-/
def tensorAdd : Py.FunDef := {
  name := `tensorAddN
  params := [
    (`a, .tensor (.var `l)),
    (`b, .tensor (.var `l))
  ]
  body := []
  retval := .add (.var `a) (.var `b)
  rettyp := .tensor (.var `l)
}

def tensorAddAltered : Py.FunDef := {
  name := `tensorAddAltered2
  params := [
    (`a, .tensor (.var `l)),
    (`b, .tensor (.var `l))
  ]
  body := []
  retval := .call (.var `tensorAddPrimOp) [(.var `a), (.var `b)]
  rettyp := .tensor (.var `l)
}

instance : HAdd (Tensor (Py.Shape.var l)) (Tensor (Py.Shape.var l)) (Tensor (Py.Shape.var l)) where
  hAdd _ _ := Tensor.mk

-- class DecidablyTrue (p : Prop) := proof : p

def AddRel (l : List Py.Shape) : Type := Unit

-- instance [DecidablyTrue (AddRel a b)] : HAdd (Tensor a) (Tensor b) (Tensor a) where
--   hAdd _ _ := Tensor.mk


def tensorAddN : Tensor (Py.Shape.var l) -> Tensor (Py.Shape.var a) -> Tensor (Py.Shape.var l) :=
  λ a b => (a + b)

class PrimOpD (r : List Py.Shape -> Type) :=
  primOpResolve {l : List Py.Shape} : Option (r l)
open PrimOpD

def tensorAddPrimOp (t : Tensor a) (t' : Tensor b) (r : AddRel [a, b]) : Tensor a := sorry

instance : PrimOpD AddRel where
  primOpResolve := .none

open Tactic

elab "my_search" : tactic => do
  Lean.Elab.Tactic.withMainContext do
    let goal ← Lean.Elab.Tactic.getMainGoal
    let goalDecl ← goal.getDecl
    let goalType := goalDecl.type
    dbg_trace f!"goal type: {goalType}"
  -- let goals <- getGoals
  -- if goals.isEmpty then return
  -- try
  --   evalTactic (← `(tactic| exact rfl))
  -- catch _ =>
  --   evalTactic (← `(tactic| apply Nat.add_comm; exact rfl))

example : AddRel [(Py.Shape.var l), (Py.Shape.var l)] := by
  my_search

#pyFunc tensorAddAltered

-- def tensorAddAltered2 :
--     Tensor (Py.Shape.var l) -> Tensor (Py.Shape.var l) -> Tensor (Py.Shape.var l) := λ a b =>
--   ((tensorAddPrimOp b) a)
