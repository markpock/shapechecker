import Lean
import ShapeChecker.Backend.Basic
import ShapeChecker.Backend.Tensors
import Mathlib.Tactic

-- def ElementwiseCompatible (a b : Py.Shape) : Prop := a = b

-- -- l ++ [a, n], l ++ [n, b]
-- inductive MatMulRel : Py.Shape → Py.Shape → Prop :=
--   -- Base case: [m, n] and [n, d]
--   | baseCase (m n d : Nat) :
--     MatMulRel
--       [m, n]
--       [n, d]
--   -- General case: l ++ [m, n] and l ++ [n, d]
--   | generalCase (l : List Nat) (m n d : Nat) :
--     MatMulRel
--       (l ++ [m, n])
--       (l ++ [n, d])
--     ->
--     MatMulRel
--       (a::l ++ [m, n])
--       (a::l ++ [n, d])

-- namespace PrimOp



-- -- Matrix multiplication operation
-- def matmul :
--   Tensor (l ++ [m, n]) →
--   Tensor (l ++ [n, d]) →
--   MatMulRel (l ++ [m, n]) (l ++ [n, d]) →
--   Tensor (l ++ [m, d]) :=
--   λ _ _ _ => .mk

-- -- variable (MMR : Py.Shape -> Py.Shape -> Py.Shape -> Type)

-- -- Elementwise addition operation
-- def add : Tensor a → Tensor b → ElementwiseCompatible a b → Tensor a :=
--   λ _ _ _ => .mk

-- end PrimOp


-- open Lean Tactic

-- syntax "resolve_prim_op_trivial" : tactic

-- macro_rules | `(tactic| resolve_prim_op_trivial) => `(tactic| omega)
-- macro_rules | `(tactic| resolve_prim_op_trivial) => `(tactic| simp; done)
-- macro_rules | `(tactic| resolve_prim_op_trivial) => `(tactic| trivial)
-- macro_rules | `(tactic| resolve_prim_op_trivial) => `(tactic| constructor)
-- macro_rules | `(tactic| resolve_prim_op_trivial) => `(tactic| simp_all)

-- macro "resolvePrimOp" : tactic => `(tactic| first
--   | assumption
--   | resolve_prim_op_trivial
--   | fail "Failed to resolve primitive operation"
-- )

-- ⟦x⟧ = ⟦y⟧ ++ ⟦z⟧
-- x ~ y ++ z

-- @[aesop safe] def MMR (A B C : Py.Shape) : Prop :=
--   ∃ l m n d,
--   A ~ l ++ [m, n] ∧
--   B ~ l ++ [n, d] ∧
--   C ~ l ++ [m, d]

-- def matmul :
--   Tensor A ->
--   Tensor B ->
--   (H : { C : Py.Shape // MMR A B C }) ->
--   Tensor H.1 :=
--   λ _ _ _ => .mk


-- def x (a : Tensor [3, 4]) (b : Tensor [4, 5]) :=
--   matmul a b (by
--     use ?_; unfold MMR;


--     use ?_; unfold MMR; exists []; exists 3; exists 4; exists 5; congr
--     simp_all only [List.nil_append, true_and]
--     have : [3, 5] = [3, 5] := by rfl
--     exact this



--     refine ⟨?C, ?pf⟩; case pf =>
--     unfold MMR
--     repeat constructor
--     have : [3, 4] = [] ++ [3, 4] := by simp
--     exact this
--     have : [4, 5] = [] ++ [4, 5] := by simp
--     constructor
--     exact this
--     have : [3, 5] = [] ++ [3, 5] := by rfl
--     exact this
--   )


-- def y (a : Tensor [3, 4]) (b : Tensor [4, 5]) :=
--   matmul a b (by refine ⟨?C, ?pf⟩; case pf =>
--     unfold MMR
--     repeat constructor
--     have : [3, 4] = [] ++ [3, 4] := by simp
--     exact this
--     have : [4, 5] = [] ++ [4, 5] := by simp
--     constructor
--     exact this
--     have : [3, 5] = [] ++ [3, 5] := by rfl
--     exact this
--   )

-- theorem silly : 2 + 1 = 3 := by simp

-- example : ∃ n, n + 1 = 3 := by
--   exists ?_
--   case refine_2 => exact silly
