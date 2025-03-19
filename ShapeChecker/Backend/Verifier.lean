import Lean
import ShapeChecker.Backend.Tensors

open Lean Tactic

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
