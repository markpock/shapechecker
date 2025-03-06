import Lean
import ShapeChecker.Backend.Basic
import ShapeChecker.Backend.AST
import ShapeChecker.Backend.Tensors
import ShapeChecker.Backend.Verifier

open Lean Elab Command Term Meta Tactic

namespace ElabPy

partial def expr (e : Py.Expr) : CommandElabM (TSyntax `term) := do
  match e with
  | .int n =>
    if n >= 0 then return quote $ Int.toNat n
    else `(- $(quote $ Int.toNat (-n)))
  | .float n => return Syntax.mkScientificLit (toString n)
  | .neg arg => `((-$(<- expr arg)))
  | .var v => return mkIdent v
  | .add left right => `(($(<- expr left) + $(<- expr right)))
  | .matmul left right => `(($(<- expr left) @ $(<- expr right)))
  | .call (.var n) args =>
    if isPrimOp n then `($(<- fncall (.var n) args.reverse) (by resolvePrimOp))
    else fncall (.var n) args.reverse
  | .call fn args =>
    fncall fn args.reverse
  | _ => throwError "Currently unimplemented" -- TODO

where fncall (fn : Py.Expr) (args : List Py.Expr) : CommandElabM (TSyntax `term) := do
  match args with
  | [] => expr fn
  | x::xs => `(($(<- fncall fn xs) $(<- expr x)))

def stmt (s : List Py.Stmt) (r : Py.Expr) : CommandElabM (TSyntax `term) := do
  match s with
  | [] => expr r
  | (.assign s ex)::xs =>`(
    let $(mkIdent s) := $(<- expr ex)
    $(<- stmt xs r)
  )

def unAnnot : Py.Shape -> CommandElabM (TSyntax `term) := λ s => do
  match s with
  | .nil => `(∅)
  | .wildcard => return mkIdent `Py.Shape.wildcard
  | .var x => `($(mkIdent `Py.Shape.var) $(mkIdent x))
  | .cons .wildcard rest =>
    `(Py.Shape.cons $(mkIdent `Py.Shape.wildcard) $(<- unAnnot rest))
  | .cons (.var x) rest =>
    `(Py.Shape.cons ($(mkIdent `Py.ShapeData.var) $(mkIdent x)) $(<- unAnnot rest))
  | .cons (.const n) rest =>
    `(Py.Shape.cons $(quote n) $(<- unAnnot rest))
  | .append op1 op2 => do
    `($(<- unAnnot op1) ++ $(<- unAnnot op2))

def typ : Py.Typ -> CommandElabM (TSyntax `term) := λ t => do
  match t with
  | .int => return mkIdent `Int
  | .float => return mkIdent `Float
  | .list t => `($(mkIdent `List) $(<- typ t))
  | .tensor annot => `($(mkIdent `Tensor) $(<- unAnnot annot))

def func : Py.FunDef -> CommandElabM (TSyntax `command) :=
  λ ⟨n, p, b, r, rt⟩ => do
  let params := if p.length > 0 then p.toArray.map Prod.fst |>.map mkIdent else #[mkIdent `_]

  `(command| def $(mkIdent n) : $(<- type (p.map Prod.snd) rt) := λ $(params)* =>
    $(<- stmt b r))

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
    elabCommand cmd
    liftTermElabM $ TryThis.addSuggestion cmd.raw {suggestion := .tsyntax cmd}
  | _ => throwUnsupportedSyntax
