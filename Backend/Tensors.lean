import Backend.Basic
import Lean

open Lean Name

def Tensor (l : Py.Shape) := { ℓ : Py.Shape // l = ℓ }
namespace Tensor
def mk : Tensor l := ⟨l, by rfl⟩
def of (l : Py.Shape) : Tensor l := ⟨l, by rfl⟩
end Tensor

def isPrimOp (n : Name) : Bool :=
  match n with
  | .str .anonymous s =>
    s == "tensorAddPrimOp" ||
    s == "tensorMatMulPrimOp"
  | _ => false
