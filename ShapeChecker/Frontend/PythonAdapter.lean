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
open Lean (Name)
inductive Expr :=
  | int (n : Int)
  | var (v : Name)
  | neg (arg : Expr)
  | add (left right : Expr)

instance : Inhabited Expr := ⟨.int 0⟩

alloy c section
typedef struct taggedUExpr {
  int tag;
  union uExpr {
    int i;
    char* var;
    struct taggedUExpr* neg;
    struct eAdd {
      struct taggedUExpr* left;
      struct taggedUExpr* right;
    } a;
  } u;
} TaggedExpr;

typedef struct expr_opt {
  bool safe;
  TaggedExpr* ptr;
} ExprOpt;

extern ExprOpt proc_expr();

// Expression type constants
#define EXPR_INT    0
#define EXPR_VAR    1
#define EXPR_NEG    2
#define EXPR_ADD    3
end

alloy c section
lean_object* traverse_expr(TaggedExpr* ptr) {
  if (ptr == NULL) {
    return lean_box(0);
  } else {
    lean_object* result;

    switch (ptr->tag) {
      case EXPR_INT: {
        result = lean_alloc_ctor(0, 1, 8);
        lean_ctor_set(result, 0, lean_box(ptr->u.i));
        break;
      }
      case EXPR_VAR: {
        printf("DEBUG: In traverse_expr, handling EXPR_VAR with value: %s\n", ptr->u.var ? ptr->u.var : "NULL");
        result = lean_alloc_ctor(1, 1, 8);
        // Create a string first
        lean_object* str = lean_mk_string(ptr->u.var ? ptr->u.var : "");
        // Then create a Name.anonymous constructor
        lean_object* name = lean_alloc_ctor(0, 0, 0);
        // Then create a Name.str constructor with the anonymous name and the string
        lean_object* name_str = lean_alloc_ctor(1, 2, 0);
        lean_ctor_set(name_str, 0, name);
        lean_ctor_set(name_str, 1, str);
        lean_ctor_set(result, 0, name_str);
        break;
      }
      case EXPR_NEG: {
        printf("DEBUG: In traverse_expr, handling EXPR_NEG\n");
        result = lean_alloc_ctor(2, 1, 8);
        lean_ctor_set(result, 0, traverse_expr(ptr->u.neg));
        break;
      }
      case EXPR_ADD: {
        printf("DEBUG: In traverse_expr, handling EXPR_ADD\n");
        result = lean_alloc_ctor(3, 2, 16);
        lean_ctor_set(result, 0, traverse_expr(ptr->u.a.left));
        lean_ctor_set(result, 1, traverse_expr(ptr->u.a.right));
        break;
      }
      default: {
        result = lean_alloc_ctor(0, 1, 8);
        lean_ctor_set(result, 0, lean_box(0));
      }
    }
    return result;
  }
}
end

alloy c extern "process_expr" def process_expr (a : Unit) : Option Expr := {
  ExprOpt o = proc_expr();

  if (!o.safe) {
    printf("Unsafe object\n");
    return lean_box(0);
  } else {
    printf("Safe object with tag: %d\n", o.ptr->tag);
    lean_object * ob = lean_alloc_ctor(1, 1, 8);
    lean_object * expr = traverse_expr(o.ptr);
    printf("DEBUG: Created Lean expression\n");
    lean_ctor_set(ob, 0, expr);
    return ob;
  }
}

inductive Tree :=
  | nil
  | branch (d : Int) (l r : Tree)

instance : Inhabited Tree := ⟨.nil⟩

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
