import Lean
import Batteries

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

-- Consider functions to have a canonical form with one return only.
structure FunDef :=
  name : Name
  params : List (Name × Typ)
  body : List Stmt
  retval : Expr
  rettyp : Typ

def Program := List FunDef

end Py

instance : Append Py.Shape where
  append a b := .app a b
notation a ":::" b => Py.Shape.cons a b
notation "∅" => Py.Shape.nil
instance : OfNat Py.ShapeData n where
  ofNat := .const n

namespace PyExample
def tN : String -> Name := (Name.str .anonymous ·)

#check (.app (.var $ tN "l") $ .cons (.var $ tN "a") $ .cons (.var $ tN "b") $ .nil : Py.Shape)
end PyExample

def Tensor (l : Py.Shape) := { ℓ : Py.Shape // l = ℓ }
namespace Tensor
def mk : Tensor l := ⟨l, by rfl⟩
def of (l : Py.Shape) : Tensor l := ⟨l, by rfl⟩
end Tensor

def ElementwiseCompatible (a b : Py.Shape) : Prop := a = b
def tensorAddPrimOp : Tensor a -> Tensor b -> ElementwiseCompatible a b -> Tensor a := λ _ _ _ => .mk

inductive MatMulRel : Py.Shape -> Py.Shape -> Prop :=
  | generalVar : MatMulRel
    (.app l $ .cons _ $ .cons (.var n) $ .nil)
    (.app l $ .cons (.var n) $ .cons _ $ .nil)
  | generalConst : MatMulRel
    (.app l $ .cons _ $ .cons (.const n) $ .nil)
    (.app l $ .cons (.const n) $ .cons _ $ .nil)

def tensorMatMulPrimOp :
  Tensor (.app l $ .cons m $ .cons n $ .nil) ->
  Tensor (.app l $ .cons n $ .cons d $ .nil) ->
  MatMulRel
    (.app l $ .cons m $ .cons n $ .nil) (.app l $ .cons n $ .cons d $ .nil) ->
  Tensor (.app l $ .cons m $ .cons d $ .nil) := λ _ _ _ => .mk

def isPrimOp (n : Name) : Bool :=
  match n with
  | .str .anonymous s =>
    s == "tensorAddPrimOp" ||
    s == "tensorMatMulPrimOp"
  | _ => false

open Elab Command Term Meta

open Tactic

syntax "resolve_prim_op_trivial" : tactic

macro_rules | `(tactic| resolve_prim_op_trivial) => `(tactic| omega)
macro_rules | `(tactic| resolve_prim_op_trivial) => `(tactic| simp; done)
macro_rules | `(tactic| resolve_prim_op_trivial) => `(tactic| trivial)
macro_rules | `(tactic| resolve_prim_op_trivial) => `(tactic| constructor)
macro_rules | `(tactic| resolve_prim_op_trivial) => `(tactic| simp_all)

macro "resolvePrimOp" : tactic => `(tactic| first
  | assumption
  | resolve_prim_op_trivial
  | fail "Failed to resolve primitive operation"
)

namespace ElabPy

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
    if isPrimOp n then `($(<- fncall (.var n) args.reverse) (by resolvePrimOp))
    else fncall (.var n) args.reverse
  | .call fn args => fncall fn args.reverse

partial def fncall (fn : Py.Expr) (args : List Py.Expr) : CommandElabM (TSyntax `term) := do
  match args with
  | [] => expr fn
  | x::xs => `(($(<- fncall fn xs) $(<- expr x)))
end

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
    `($(mkIdent `Py.Shape.wildcard) ::: $(<- unAnnot rest))
  | .cons (.var x) rest =>
    `(($(mkIdent `Py.ShapeData.var) $(mkIdent x)) ::: $(<- unAnnot rest))
  | .cons (.const n) rest =>
    `($(quote n) ::: $(<- unAnnot rest))
  | .app op1 op2 => do
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

  `(def $(mkIdent n) : $(<- type (p.map Prod.snd) rt) := λ $(params)* =>
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
    -- let f : Format <- liftCoreM $ PrettyPrinter.ppCommand cmd
    -- dbg_trace s!"{f}"
    elabCommand cmd
    liftTermElabM $ Tactic.TryThis.addSuggestion cmd.raw {suggestion := .tsyntax cmd}
  | _ => throwUnsupportedSyntax
