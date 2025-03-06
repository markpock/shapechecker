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

-- extern int hw();
-- extern Opt hw2();
extern Opt proc();

end

alloy c opaque_extern_type S => BTNode where
  -- foreach(s, f) := lean_inc(f); lean_apply_1(f, s->m_s)
  toLean => "CFoo_to_lean"
  ofLean => "of_LeanFoo"
  hw => "sad"
  finalize(s) := free(s)

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

-- alloy c opaque_extern_type COpt => Opt where
--   toLean => "COpt_to_lean"
--   ofLean => "of_lean_COpt"
--   is_safe(s) := if (s.safe) {
--     return true
--   } else {
--     return false
--   }
--   finalize(s) := {}


inductive Tree :=
  | nil
  | branch (d : Int) (l r : Tree)

instance : Inhabited Tree := ⟨.nil⟩

alloy c extern "ct" def ct (a : Unit) : Option Tree := {
  printf("Entering ct\n");
  Opt o = proc();
  printf("Exited proc\n");
  if (!o.safe) {
    return lean_box(0);
  } else {
    lean_object * ob = lean_alloc_ctor(1, 1, 8);
    lean_ctor_set(ob, 0, traverse(o.ptr));
    return ob;
  }
}
