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


-- Simplified Expr definition with basic types
inductive Expr :=
  | int (n : Int)
  | float (f : Float)
  | neg (arg : Expr)
  | var (v : Name)

instance : Inhabited Expr := ⟨.int 0⟩

-- Add to PythonAdapter.lean after the existing C sections
alloy c section
typedef struct expr_node_s {
  int integer;
  double float_val;
  struct expr_node_s *neg;
  char *var;
  int expr_type;
} ExprNode;

typedef struct expr_opt {
  bool safe;
  ExprNode* ptr;
} ExprOpt;

extern ExprOpt proc_expr();

// Expression type constants
#define EXPR_INT    0
#define EXPR_FLOAT  1
#define EXPR_NEG    2
#define EXPR_VAR    3
end

alloy c section
lean_object* traverse_expr(ExprNode* ptr) {
  if (ptr == NULL) {
    return lean_box(0);
  } else {
    lean_object* result;

    switch (ptr->expr_type) {
      case EXPR_INT: {
        // Create int expr
        result = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(result, 0, lean_box_int(ptr->integer));
        break;
      }

      case EXPR_FLOAT: {
        // Create float expr
        result = lean_alloc_ctor(1, 1, 0);
        lean_ctor_set(result, 0, lean_box_float(ptr->float_val));
        break;
      }

      case EXPR_NEG: {
        // Create neg expr
        result = lean_alloc_ctor(2, 1, 0);
        lean_ctor_set(result, 0, traverse_expr(ptr->neg));
        break;
      }

      case EXPR_VAR: {
        // Create var expr
        result = lean_alloc_ctor(3, 1, 0);
        lean_object* name = lean_mk_string(ptr->var ? ptr->var : "");
        lean_ctor_set(result, 0, name);
        break;
      }

      default: {
        // Default to int(0) for unknown types
        result = lean_alloc_ctor(0, 1, 0);
        lean_ctor_set(result, 0, lean_box_int(0));
      }
    }

    return result;
  }
}
end

alloy c extern "process_expr" def process_expr (a : Unit) : Option Expr := {
  printf("Entering process_expr\n");
  ExprOpt o = proc_expr();
  printf("Exited proc_expr\n");
  if (!o.safe) {
    return lean_box(0);
  } else {
    lean_object * ob = lean_alloc_ctor(1, 1, 8);
    lean_ctor_set(ob, 0, traverse_expr(o.ptr));
    return ob;
  }
}


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
