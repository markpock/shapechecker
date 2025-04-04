import Lean
import ShapeChecker.Backend.Basic
import ShapeChecker.Backend.AST
import ShapeChecker.Backend.Verifier

open Lean Elab Command Term Meta Tactic

def PrimOp.name : PrimOp -> Name
  | .add => `Tensor.PrimOp.add
  | .matmul => `Tensor.PrimOp.matmul

namespace ElabPy

#check Tensor.mk

def unAnnot : Py.Shape -> CommandElabM (TSyntax `term) := λ s => do
  match s with
  | .nil => `($(mkIdent `Py.Shape.nil))
  | .var x => `($(mkIdent `Py.Shape.var) $(mkIdent x))
  | .lift (.var x) =>
    `($(mkIdent `Py.Shape.lift) ($(mkIdent `Py.ShapeData.var) $(mkIdent x)))
  | .lift (.const n) =>
    `($(mkIdent `Py.Shape.lift) $(quote n))
  | .append op1 op2 =>
    `($(mkIdent `Py.Shape.append) ($(<- unAnnot op1)) ($(<- unAnnot op2)))

partial def expr (e : Py.Expr) : CommandElabM (TSyntax `term) := do
  match e with
  | .int n =>
    if n >= 0 then return quote n.toNat
    else `(- $(quote (-n).toNat))
  | .float n => return Syntax.mkScientificLit (toString n)
  | .neg arg => `((-$(<- expr arg)))
  | .var v => return mkIdent v
  | .add left right => `($(mkIdent `Tensor.PrimOp.add) ($(<- expr left)) ($(<- expr right)) (by resolvePrimOp))
  | .matmul left right => `($(mkIdent `Tensor.PrimOp.matmul) ($(<- expr left)) ($(<- expr right)) (by resolvePrimOp))
  | .call (.var n) args =>
    match isPrimOp n with
    | .some k => `($(<- fncall (.inl $ k.name) args.reverse) (by resolvePrimOp))
    | .none => fncall (.inr $ .var n) args.reverse
  | .call fn args => fncall (.inr fn) args.reverse
  | .list l => list l
  | .ones l | .zeros l | .random l => `($(mkIdent `Tensor.of) $(<- unAnnot l))
  | _ => throwError "Currently unimplemented" -- TODO
where
  fncall (fn : Name ⊕ Py.Expr) (args : List Py.Expr) : CommandElabM (TSyntax `term) := do
    match args with
    | [] => match fn with
      | .inl n => `($(mkIdent n))
      | .inr v => `($(<- expr v))
    | x::xs => `(($(<- fncall fn xs) $(<- expr x)))
  list (l : List Py.Expr) : CommandElabM (TSyntax `term) := do
    match l with
    | [] => `($(mkIdent `List.nil))
    | x::xs => `($(mkIdent `List.cons) $(<- expr x) ($(<- list xs)))

partial def functionBody (s : List Py.Stmt) (r : Py.Expr) : CommandElabM (TSyntax `Lean.Parser.Term.doSeq)
  := stmtList (s.map .inl) (.inr r)
where
  stmt (s : Py.Stmt ⊕ Py.Expr) : CommandElabM (TSyntax `Lean.Parser.Term.doSeqItem) :=
    do match s with
    | .inl $ .declare n exp =>
      `(Lean.Parser.Term.doSeqItem | $(<- `(Lean.Parser.Term.doLet | let mut $(mkIdent n) := $(<- expr exp))):doLet)
    | .inl $ .mutate n exp =>
      `(Lean.Parser.Term.doSeqItem | $(<- `(Lean.Parser.Term.doReassign | $(mkIdent n) := $(<- expr exp))):doReassign)
    | .inl $ .for n l s r =>
      `(Lean.Parser.Term.doSeqItem | for $(mkIdent n) in $(<- expr l) do
        $(<- stmtList (s.map .inl) (.inl r))
      )
    | .inr exp => `(Lean.Parser.Term.doSeqItem | return $(<- expr exp))
  stmtList (s : List (Py.Stmt ⊕ Py.Expr)) (r' : (Py.Stmt ⊕ Py.Expr)) : CommandElabM (TSyntax `Lean.Parser.Term.doSeq) := do
    let mut res := #[]
    for e in s ++ [r'] do
      let e <- stmt e
      res := res.push e
    `(doSeq | $res*)

def typ : Py.Typ -> CommandElabM (TSyntax `term) := λ t => do
  match t with
  | .int => return mkIdent `Int
  | .float => return mkIdent `Float
  | .list t => `($(mkIdent `List) $(<- typ t))
  | .tensor annot => `($(mkIdent `Tensor) $(<- unAnnot annot))

def func : Py.FunDef -> CommandElabM (TSyntax `command) := λ ⟨n, p, b, r, rt⟩ => do
  let params := if p.length > 0 then p.toArray.map Prod.fst |>.map mkIdent else #[mkIdent `_]
  let body <- functionBody b r
  `(command| def $(mkIdent n) : $(<- type (p.map Prod.snd) rt) := λ $(params)* => $(mkIdent `Id.run) do $(body))
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
