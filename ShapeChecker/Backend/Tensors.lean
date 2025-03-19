import ShapeChecker.Backend.Basic
import Lean

open Lean Name

def Tensor (l : Py.Shape) := { ℓ : Py.Shape // l = ℓ }
namespace Tensor
def mk : Tensor l := ⟨l, by rfl⟩
def of (l : Py.Shape) : Tensor l := ⟨l, by rfl⟩
end Tensor

inductive PrimOp :=
  | add
  | matmul

def isPrimOp (n : Name) : Option PrimOp :=
  match n with
  | .str .anonymous s =>
    if      s == "tensorAddPrimOp"    then return .add
    else if s == "tensorMatMulPrimOp" then return .matmul
    else throw ()
  | _ => throw ()

def reverse (s : Py.Shape) : Py.Shape :=
  match s with
  | .var x => sorry
  | .append a b => sorry
  | .cons (.const k) l => sorry
  | .cons (.var v) l => sorry
  | .nil => .nil
