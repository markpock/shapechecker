import Lean
import ShapeChecker.Backend.Basic
-- import Mathlib
-- import Mathlib.Tactic
-- import Mathlib.Data.Finset.Basic

open Lean (Name)

-- namespace Py
-- namespace Shape
-- @[simp] def listVars : Shape -> Finset Name
--   | .nil | .lift _ => ∅
--   | .append s₁ s₂ => s₁.listVars ∪ s₂.listVars
--   | .var x => {x}

-- @[simp] def dataVars : Shape -> Finset Name
--   | .lift (.var x) => {x}
--   | .append s₁ s₂ => s₁.dataVars ∪ s₂.dataVars
--   | _ => ∅

-- def safe (f : Name -> Option V) (s : Finset Name) := ∀ x ∈ s, f x ≠ .none

-- def safe.left : safe f (s₁ ∪ s₂) -> safe f s₁ := by
--   unfold safe; intros H x h; apply H; simp; tauto

-- def safe.right : safe f (s₁ ∪ s₂) -> safe f s₂ := by
--   unfold safe; intros H x h; apply H; simp; tauto

-- @[simp] def eval (s : Shape) (lv : Name -> Option (List Nat)) (dv : Name -> Option Nat) :
--   safe lv s.listVars -> safe dv s.dataVars -> List Nat := λ h1 h2 => match h : s with
--   | .nil => .nil
--   | .lift (.const k) => [k]
--   | .lift (.var v) => match h' : dv v with
--     | .some x => [x]
--     | .none => by simp at h2; unfold safe at h2; simp_all
--   | .append s₁ s₂ => by simp_all; exact (
--     s₁.eval lv dv h1.left h2.left ++
--     s₂.eval lv dv h1.right h2.right)
--   | .var v => match h' : lv v with
--     | .some x => x
--     | .none => by simp at h1; unfold safe at h1; simp_all

-- def equiv (s₁ s₂ : Shape) := ∀ lv₁ lv₂ dv₁ dv₂,
--   (s1 : safe lv₁ s₁.listVars) -> (s2 : safe dv₁ s₁.dataVars) ->
--   (s3 : safe lv₂ s₂.listVars) -> (s4 : safe dv₂ s₂.dataVars) ->
--   (∀ x ∈ s₁.dataVars ∩ s₂.dataVars, dv₁ x = dv₂ x) ->
--   (∀ x ∈ s₁.listVars ∩ s₂.listVars, lv₁ x = lv₂ x) ->
--   s₁.eval lv₁ dv₁ s1 s2 = s₂.eval lv₂ dv₂ s3 s4

-- def equiv.refl : ∀ s : Shape, s.equiv s := by
--   let this : ∀ {α} {x : α}, ∀ f : False, f.elim = x := by tauto
--   intro s; unfold Shape.equiv; intro lv₁ lv₂ dv₁ dv₂ s1 s2 s3 s4 interD interL
--   induction s <;> (try simp_all)
--   case var =>
--     split; swap; apply this
--     split; swap; symm; apply this
--     simp_all
--   case lift data => cases data <;> simp_all; case var =>
--     split; swap; apply this
--     split; swap; symm; apply this
--     simp_all
--   case append op1 op2 ih1 ih2 =>
--     have : ∀ {l1 l2 l3 l4 : List Nat}, l1 = l2 -> l3 = l4 -> l1 ++ l3 = l2 ++ l4 := by simp
--     apply this
--     apply ih1
--     apply ih2

-- def equiv.symm : ∀ s1 s2 : Shape, s1.equiv s2 -> s2.equiv s1 := by
--   unfold Shape.equiv
--   intros s1 s2 eq lv₁ lv₂ dv₁ dv₂ s1 s2 s3 s4 interD interL; symm
--   apply eq <;> (rw [Finset.inter_comm]; intros; symm)
--   apply interD; assumption
--   apply interL; assumption

-- -- @[aesop safe] def heuristic (A : Shape) : A.equiv (.append .nil A) := by
-- --   unfold Shape.equiv; intros lv₁ lv₂ dv₁ dv₂ s1 s2 s3 s4 interD interL
-- --   rw [Shape.eval, Shape.eval]; simp
-- --   simp at interD; simp at interL
-- --   apply equiv.refl
-- --   simp; assumption
-- --   simp; assumption

-- end Shape
-- end Py


def ElementwiseCompatible (a b c : Py.Shape) : Prop :=
  a = b /\ b = c
  -- a.equiv b /\ b.equiv c

namespace Tensor
namespace PrimOp

def add (_ : Tensor l₁) (_ : Tensor l₂) (r : { l₃ // ElementwiseCompatible l₁ l₂ l₃ }) : Tensor r.1 := .mk

end PrimOp
end Tensor
