import Lean
-- import Mathlib.Data.Finset.Basic
-- import Mathlib.Data.Fintype.Basic

-- open Lean


-- @[simp] abbrev K (x : α) (_ : _β) := x

-- namespace Py

-- inductive ShapeData :=
--   | var (x : Name)
--   | const (n : Nat)

-- inductive Shape :=
--   | nil
--   | lift (data : ShapeData)
--   | append (op1 op2 : Shape)
--   | var (x : Name)
--   -- | reverse (s : Shape)

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

-- @[aesop safe] def heuristic (A : Shape) : A.equiv (.append .nil A) := by
--   unfold Shape.equiv; intros lv₁ lv₂ dv₁ dv₂ s1 s2 s3 s4 interD interL
--   rw [Shape.eval, Shape.eval]; simp
--   simp at interD; simp at interL
--   apply equiv.refl
--   simp; assumption
--   simp; assumption

-- end Shape

-- end Py

-- instance {n : Nat} : OfNat Py.ShapeData n where
--   ofNat := .const n

-- example :
--   let A : Py.Shape := .append (.lift $ .const 3) (.lift $ .const 5)
--   A.equiv (.append .nil A) := by
--   intro A
--   simp_all only [A]
--   apply Py.Shape.heuristic

  -- congr <;> funext
  -- apply interL
  -- apply interD



-- def Association (s : Finset Name) (V : Type*) :=
--   { f : Name -> Option V // ∀ x ∈ s, f x ≠ .none }




-- def Association (s : Finset Name) (V : Type*) := { x // x ∈ s } -> V

-- def Association.breakLeft (a : Association (P ∪ Q) V) : Association P V := by
--   intro x; apply a; cases x; case mk val prop => exists val; simp; left; assumption

-- def Association.breakRight (a : Association (P ∪ Q) V) : Association Q V := by
--   rw [Finset.union_comm P Q] at a; apply Association.breakLeft; apply a

-- @[simp] def Shape.eval (s : Shape) : Association s.listVars (List Nat) -> Association s.dataVars (Nat) -> List Nat :=
--   match s with
--   | .nil => K $ K .nil
--   | .lift (.const k) => K $ K [k]
--   | .lift (.var n) => λ _ a => [a ⟨n, by simp⟩]
--   | .append s₁ s₂  => λ a b => by
--     simp at a; simp at b;
--     apply List.append; apply s₁.eval
--     apply Association.breakLeft; assumption
--     apply Association.breakLeft; assumption
--     apply s₂.eval
--     apply Association.breakRight; assumption
--     apply Association.breakRight; assumption
--   | .var n => λ a _ => a ⟨n, by simp⟩

-- ⟦C⟧ = ⟦A⟧
-- C ~ A

-- ⟦C⟧ = ⟦A⟧ ++ ⟦B⟧
-- C ~ A ++ B

-- ⟦[3, 5]⟧ = ⟦[]⟧ ++ ⟦[3, 5]⟧
-- [3, 5] ~ [] ++ [3, 5]

-- ⟦rev C⟧ ~ rev ⟦A⟧
-- ⟦rev C⟧ ~ ⟦rev A⟧

-- ⟦swap C⟧ ~ swap ⟦A⟧
-- ⟦swap C⟧ ~ ⟦swap A⟧

-- ⟦sum C⟧ ~ sum ⟦A⟧
-- ⟦sum C⟧ ~ ⟦sum A⟧


-- open scoped symmDiff
-- def Association.restrict (a : Association S V) (s : Finset Name) : Association (S ∩ s) V := by
--   intro x; apply a; cases x; case mk val prop => simp at prop; tauto

-- def Shape.equiv (s₁ s₂ : Shape) :=
--   ∀ a₁ : Association s₁.dataVars Nat,
--   ∀ a₂ : Association s₂.dataVars Nat,
--   ∀ b₁ : Association s₁.listVars (List Nat),
--   ∀ b₂ : Association s₂.listVars (List Nat),
--   (∀ x pf1 pf2, a₁.restrict s₂.dataVars ⟨x, pf1⟩ = a₂.restrict s₁.dataVars ⟨x, pf2⟩) ->
--   (∀ x pf1 pf2, b₁.restrict s₂.listVars ⟨x, pf1⟩ = b₂.restrict s₁.listVars ⟨x, pf2⟩) ->
--   s₁.eval b₁ a₁ = s₂.eval b₂ a₂

/-
Proof by authority.
-/
-- def heuristic (A : Shape) : A.equiv (.append .nil A) := by

--   unfold Shape.equiv; intros a b c d e f
--   rw [Shape.eval]
--   simp_all
--   congr
--   funext x; cases x; case _ val prop =>


--   simp_rw [Finset.empty_union] at e





-- theorem Shape.equiv.refl {s₁ : Shape} : s₁.equiv s₁ := by
--   unfold Shape.equiv; intros

--   sorry
-- theorem Shape.equiv.symm := sorry
-- theorem Shape.equiv.trans := sorry




/-
∀ a₁ : Association s₁.vars (List Nat),
∀ a₂ : Association s₂.vars (List Nat),
a₁.restrict s₂.vars = a₂.restrict s₁.vars ->
s₁.eval a₁ = s₂.eval a₂
-/
