import Alloy.C


open scoped Alloy.C

alloy c include <lean/lean.h>
alloy c include "stdio.h" "stdbool.h"

alloy c section

typedef struct binary_tree_node_s {
  int data;
  struct binary_tree_node_s *left, *right;
} BTNode;

typedef struct opt {
  bool safe;
  BTNode* ptr;
} Opt;

extern Opt proc();

end

alloy c section

lean_object* traverse(BTNode* ptr) {
  if (ptr == NULL) {
    return lean_box(0);
  } else {
    lean_object * lo = lean_alloc_ctor(1, 3, 8 * 3);
    lean_ctor_set(lo, 0, lean_box(ptr->data));
    lean_ctor_set(lo, 1, traverse(ptr->left));
    lean_ctor_set(lo, 2, traverse(ptr->right));
    return lo;
  }
}
end

inductive Tree :=
  | nil
  | branch (d : Int) (l r : Tree)
instance : Inhabited Tree := ⟨.nil⟩

@[match_pattern] def Tree.singleton (data : Int) : Tree := .branch data .nil .nil

def Tree.toString : Tree -> String
  | .nil => "·"
  | .singleton data => s!"{data}"
  | .branch data left right => s!"{data}, \{{left.toString}} \{{right.toString}}"

instance : ToString Tree := ⟨Tree.toString⟩

alloy c extern "ct" def ct (a : Unit) : Option Tree := {
  Opt o = proc();
  if (!o.safe) {
    return lean_box(0);
  } else {
    lean_object * ob = lean_alloc_ctor(1, 1, 8);
    lean_ctor_set(ob, 0, traverse(o.ptr));
    return ob;
  }
}
