import Lean

namespace Py

inductive ShapeData :=
  | var (x : Lean.Name)
  | const (n : Nat)

inductive Shape :=
  | nil
  | lift (data : ShapeData)
  | append (op1 op2 : Shape)
  | var (x : Lean.Name)
end Py
instance {n : Nat} : OfNat Py.ShapeData n where ofNat := .const n

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
