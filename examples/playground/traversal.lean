import Batteries

abbrev ℕ := Nat

inductive Tree (α : Type)
  | leaf
  | branch (data : α) (left : Tree α) (right : Tree α)

@[match_pattern] def Tree.singleton (data : α) : Tree α := .branch data .leaf .leaf

def Tree.toString [ToString α] : Tree α -> String
  | .leaf => "·"
  | .singleton data => s!"{data}"
  | .branch data left right => s!"{data}, \{{left.toString}} \{{right.toString}}"

instance [ToString α] : ToString (Tree α) := ⟨Tree.toString⟩

def Tree.reverse : Tree α -> Tree α
  | .leaf => .leaf
  | .branch d l r => .branch d r l

def Tree.reverse.requireTwoChildren (tree : Tree α) : Option (Tree α) :=
  match tree with
  | .leaf => throw ()
  | .singleton data => return .singleton data
  | .branch data left right => return .branch data right left

def exa : Tree ℕ := .branch 3 (.singleton 2) (.leaf)

#eval exa
#eval exa.reverse
#eval Tree.reverse.requireTwoChildren exa
