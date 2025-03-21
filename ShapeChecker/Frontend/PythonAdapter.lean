import Alloy.C

open scoped Alloy.C
open Lean (Name)

-- Core Lean definitions

namespace Adapter
inductive Expr :=
  | int (n : Int)
  | var (v : Name)
  | neg (arg : Expr)
  | add (left right : Expr)
  | matMult (left right : Expr)
  | tensor (dim1 dim2 : Expr)
  deriving Repr

instance : Inhabited Expr := ⟨.int 0⟩

inductive Stmt :=
  | assign (name : Name) (value : Expr)
  | ret (value : Expr)
  deriving Repr

instance : Inhabited Stmt := ⟨.ret (.int 0)⟩

structure Annotations where
  inputA : String
  inputB : String
  output : String
  deriving Repr

structure FunDef where
  name : String
  body : List Stmt
  inputAnnotations : List (String × String)
  outputAnnotation : String
  deriving Repr

end Adapter

@[extern "c_process_function"]
opaque process_function : Unit → Option Adapter.FunDef

-- C bindings module
namespace PythonBindings
  alloy c include <lean/lean.h>
  alloy c include "stdio.h" "stdbool.h"
  alloy c section
  -- Constants
  #define EXPR_INT    0
  #define EXPR_VAR    1
  #define EXPR_NEG    2
  #define EXPR_ADD    3
  #define EXPR_MATMULT 4
  #define EXPR_TENSOR  5

  #define STMT_ASSIGN  0
  #define STMT_RETURN  1

  -- Forward declarations
  struct taggedUExpr;
  struct taggedUStmt;
  struct CParameter;
  struct CFunction;
  struct function_opt;

  -- Type definitions
  typedef struct taggedUExpr TaggedExpr;
  typedef struct taggedUStmt TaggedStmt;
  typedef struct function_opt FunctionOpt;
  typedef struct CParameter CParameter;
  typedef struct CFunction CFunction;

  -- Struct definitions
  struct taggedUExpr {
    int tag;
    union {
      int i;
      char* var;
      TaggedExpr* neg;
      struct {
        TaggedExpr* left;
        TaggedExpr* right;
      } a;
      struct {
        TaggedExpr* left;
        TaggedExpr* right;
      } matMult;
      struct {
        TaggedExpr* dim1;
        TaggedExpr* dim2;
      } tensor;
    } u;
  };

  struct taggedUStmt {
    int tag;
    union {
      struct {
        char* name;
        TaggedExpr* value;
      } assign;
      struct {
        TaggedExpr* value;
      } ret;
    } u;
  };

  struct CParameter {
    char* name;
    TaggedExpr* type;
  };

  struct CFunction {
    char* name;
    CParameter* params;
    int param_count;
    TaggedExpr* return_type;
    TaggedStmt** body;         // Array of statements
    int body_count;            // Count of statements
    struct {
      char* input_a;
      char* input_b;
      char* output;
    } annotations;
  };

  struct function_opt {
    bool safe;
    void* ptr;
  };

  -- Function declarations
  extern FunctionOpt proc_function();
  extern lean_object* traverse_expr(TaggedExpr* ptr);
  extern lean_object* traverse_stmt(TaggedStmt* ptr);
  extern lean_object* traverse_stmt_list(TaggedStmt** stmts, size_t count);
  extern lean_object* lean_traverse_function(void* ptr);

  lean_object* process_function(lean_object* unit) {
    printf("DEBUG: Starting process_function\n");
    FunctionOpt o = proc_function();

    if (!o.safe) {
      printf("Unsafe function\n");
      lean_object* none = lean_box(0);
      lean_inc_ref(none);  // Increment reference count for none
      return none;
    } else {
      printf("Safe function\n");
      lean_object* ob = lean_alloc_ctor(1, 1, 8);
      lean_inc_ref(ob);  // Increment reference count for option constructor
      printf("DEBUG: Created option constructor\n");
      lean_object* func = lean_traverse_function(o.ptr);
      if (func == lean_box(0)) {
        printf("ERROR: Failed to traverse function\n");
        lean_dec_ref(ob);
        lean_object* none = lean_box(0);
        lean_inc_ref(none);
        return none;
      }
      printf("DEBUG: Created Lean function\n");
      lean_ctor_set(ob, 0, func);
      printf("DEBUG: Set option value\n");
      return ob;
    }
  }
  end
end PythonBindings
