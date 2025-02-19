

abbrev ℕ := Nat

inductive Tree (α : Type)
  | leaf
  | branch (data : α) (left : Tree α) (right : Tree α)

-- 3
#check (.branch 3 (.leaf) (.leaf) : Tree ℕ)

/-
  3
2
-/
#check (.branch 3 (.branch 2 (.leaf) (.leaf)) (.leaf) : Tree ℕ)

def Tree.reverse (tree : Tree α) : Option (Tree α) :=
  match tree with
  | .leaf => .some .leaf
  | _ => .none
  -- | .branch data left right => .branch data right left

def exa : Tree ℕ := .branch 3 (.branch 2 (.leaf) (.leaf)) (.leaf)

/-
  3
   2
-/
#eval Tree.reverse exa
