#include <Python.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <string.h>
#include <lean/lean.h>

// ==========================================================================
// Constants and Type Definitions
// ==========================================================================

// Expression type constants
#define EXPR_INT    0
#define EXPR_VAR    1
#define EXPR_NEG    2
#define EXPR_ADD    3
#define EXPR_MATMULT 4
#define EXPR_TENSOR  5

// Statement type constants
#define STMT_ASSIGN  0
#define STMT_RETURN  1

// Convenience macros for returning function options
#define FunctionNone ((FunctionOpt){false, NULL})
#define FunctionSome(x) ((FunctionOpt){true, x})

// Forward declarations
struct TaggedExpr;
struct TaggedStmt;
void printExpr(struct TaggedExpr *expr);
void printStmt(struct TaggedStmt *stmt);

// Define the TaggedExpr structure
typedef struct TaggedExpr {
  int tag;
  union uExpr {
    int i;
    char* var;
    struct TaggedExpr* neg;
    struct {
      struct TaggedExpr* left;
      struct TaggedExpr* right;
    } a;
    struct {
      struct TaggedExpr* left;
      struct TaggedExpr* right;
    } matMult;
    struct {
      struct TaggedExpr* dim1;
      struct TaggedExpr* dim2;
    } tensor;
  } u;
} TaggedExpr;

// Define the TaggedStmt structure
typedef struct TaggedStmt {
  int tag;
  union uStmt {
    struct {
      char* name;
      struct TaggedExpr* value;
    } assign;
    struct {
      struct TaggedExpr* value;
    } ret;
  } u;
} TaggedStmt;

// Structure to represent a parameter
typedef struct Parameter {
    char* name;                 // Parameter name
    struct TaggedExpr* type;    // Parameter type
} Parameter;

// Define a single Function structure
typedef struct Function {
    char* name;                 // Function name
    Parameter* params;          // Array of parameters
    int param_count;            // Number of parameters
    struct TaggedExpr* return_type; // Return type
    struct TaggedStmt** body;   // Array of statements
    int body_count;             // Number of statements
    struct {
        char* input_a;          // Input annotation for parameter a
        char* input_b;          // Input annotation for parameter b
        char* output;           // Output annotation
    } annotations;
} Function;

typedef struct function_opt {
    bool safe;
    void* ptr;
} FunctionOpt;

// Forward declarations for Lean interface functions
lean_object* traverse_expr(PyObject* pyExpr);
lean_object* traverse_stmt(PyObject* pyStmt);
lean_object* traverse_stmt_list(PyObject* pyStmtList);
lean_object* lean_traverse_function(PyObject* pyFunction);

// ==========================================================================
// Helper Functions
// ==========================================================================

// Safe version of strdup
char* strdup_safe(const char* str) {
    if (str == NULL) return NULL;
    char* new_str = strdup(str);
    if (new_str == NULL) {
        fprintf(stderr, "Memory allocation failed\n");
        exit(1);
    }
    return new_str;
}

// ==========================================================================
// Debug Print Functions
// ==========================================================================

// Print an expression (for debugging)
void printExpr(struct TaggedExpr *expr) {
    if (expr == NULL) {
        printf("NULL expression");
        return;
    }
    
    switch (expr->tag) {
        case EXPR_INT:
            printf("%d", expr->u.i);
            break;
        case EXPR_VAR:
            printf("%s", expr->u.var);
            break;
        case EXPR_NEG:
            printf("-(");
            printExpr(expr->u.neg);
            printf(")");
            break;
        case EXPR_ADD:
            printf("(");
            printExpr(expr->u.a.left);
            printf(" + ");
            printExpr(expr->u.a.right);
            printf(")");
            break;
        case EXPR_MATMULT:
            printf("(");
            printExpr(expr->u.matMult.left);
            printf(" @ ");
            printExpr(expr->u.matMult.right);
            printf(")");
            break;
        case EXPR_TENSOR:
            printf("Tensor(");
            printExpr(expr->u.tensor.dim1);
            printf(", ");
            printExpr(expr->u.tensor.dim2);
            printf(")");
            break;
        default:
            printf("Unknown expression type: %d", expr->tag);
            break;
    }
}

// Print a statement (for debugging)
void printStmt(struct TaggedStmt *stmt) {
    if (stmt == NULL) {
        printf("NULL statement");
        return;
    }
    
    switch (stmt->tag) {
        case STMT_ASSIGN:
            printf("%s = ", stmt->u.assign.name);
            printExpr(stmt->u.assign.value);
            break;
        case STMT_RETURN:
            printf("return ");
            printExpr(stmt->u.ret.value);
            break;
        default:
            printf("Unknown statement type: %d", stmt->tag);
            break;
    }
}

// Print a function definition (for debugging)
void printFunction(Function* func) {
    if (func == NULL) {
        printf("NULL Function\n");
        return;
    }

    printf("\n=== FUNCTION DEFINITION ===\n");
    printf("Function %s(\n", func->name);
    
    // Print parameters
    for (int i = 0; i < func->param_count; i++) {
        printf("  %s: ", func->params[i].name);
        printExpr(func->params[i].type);
        if (i < func->param_count - 1)
            printf(",\n");
        else
            printf("\n");
    }
    
    printf(") -> ");
    printExpr(func->return_type);
    printf(" {\n");
    
    // Print body
    for (int i = 0; i < func->body_count; i++) {
        printf("  ");
        printStmt(func->body[i]);
        printf("\n");
    }
    printf("}\n");
    printf("=========================\n\n");
}

// ==========================================================================
// Python Interface Functions
// ==========================================================================

// Initialize Python interpreter
bool init_python() {
    Py_Initialize();
    if (!Py_IsInitialized()) {
        fprintf(stderr, "Failed to initialize Python interpreter\n");
        return false;
    }
    
    // Add the scripts directory to Python's path
    PyRun_SimpleString(
        "import sys\n"
        "import os\n"
        "current_dir = os.getcwd()\n"
        "print(f'Working directory: {current_dir}')\n"
        "scripts_dir = os.path.join(current_dir, 'examples', 'python-scripts')\n"
        "if os.path.exists(scripts_dir):\n"
        "    print(f'Adding scripts directory to path: {scripts_dir}')\n"
        "    sys.path.insert(0, scripts_dir)\n"
        "else:\n"
        "    print(f'Scripts directory not found: {scripts_dir}')\n"
        "    # Try parent directory's examples/python-scripts\n"
        "    parent_dir = os.path.dirname(current_dir)\n"
        "    scripts_dir = os.path.join(parent_dir, 'examples', 'python-scripts')\n"
        "    if os.path.exists(scripts_dir):\n"
        "        print(f'Adding parent scripts directory to path: {scripts_dir}')\n"
        "        sys.path.insert(0, scripts_dir)\n"
        "    else:\n"
        "        print(f'Parent scripts directory not found: {scripts_dir}')\n"
        "print(f'Python path: {sys.path}')\n"
    );
    
    return true;
}

// Get parsed function from Python
PyObject* get_python_function() {
    // Import the script module
    PyObject* scriptModule = PyImport_ImportModule("script");
    if (!scriptModule) {
        fprintf(stderr, "Failed to import script module\n");
        PyErr_Print();
        return NULL;
    }
    
    // Get the parsed function from the module
    PyObject* getParsedFunction = PyObject_GetAttrString(scriptModule, "get_parsed_function");
    if (!getParsedFunction || !PyCallable_Check(getParsedFunction)) {
        fprintf(stderr, "Failed to get get_parsed_function function\n");
        Py_XDECREF(scriptModule);
        return NULL;
    }
    
    // Call the function to get the parsed function
    PyObject* parsedFunction = PyObject_CallObject(getParsedFunction, NULL);
    if (!parsedFunction) {
        fprintf(stderr, "Failed to call get_parsed_function\n");
        Py_XDECREF(getParsedFunction);
        Py_XDECREF(scriptModule);
        return NULL;
    }
    
    Py_XDECREF(getParsedFunction);
    Py_XDECREF(scriptModule);
    
    return parsedFunction;
}

// Main entry point called from Lean
FunctionOpt proc_function() {
    if (!init_python()) {
        return FunctionNone;
    }
    
    PyObject* func = get_python_function();
    if (func == NULL) {
        fprintf(stderr, "Failed to get parsed function from Python\n");
        return FunctionNone;
    }
    
    // Print function details for debugging
    PyObject* name = PyObject_GetAttrString(func, "name");
    if (name && PyUnicode_Check(name)) {
        printf("Function name: %s\n", PyUnicode_AsUTF8(name));
        Py_XDECREF(name);
    }
    
    return FunctionSome(func);
}

// ==========================================================================
// Lean Objects Creation Functions
// ==========================================================================

// Convert a Python expression to a Lean object
lean_object* traverse_expr(PyObject* pyExpr) {
    if (!pyExpr || pyExpr == Py_None) {
        return lean_box(0);
    }
    
    PyObject* tag = PyObject_GetAttrString(pyExpr, "tag");
    if (!tag || !PyLong_Check(tag)) {
        fprintf(stderr, "Invalid expression tag\n");
        Py_XDECREF(tag);
        return lean_box(0);
    }
    
    int expr_tag = PyLong_AsLong(tag);
    Py_XDECREF(tag);
    
    lean_object* result;
    
    switch (expr_tag) {
        case EXPR_INT: {
            PyObject* value = PyObject_GetAttrString(pyExpr, "value");
            if (!value || !PyLong_Check(value)) {
                fprintf(stderr, "Invalid integer expression\n");
                Py_XDECREF(value);
                return lean_box(0);
            }
            
            int val = PyLong_AsLong(value);
            Py_XDECREF(value);
            
            printf("Creating Lean object for integer: %d\n", val);
            result = lean_alloc_ctor(0, 1, 0); // Constructor tag 0 for Int
            lean_ctor_set(result, 0, lean_box(val));
            break;
        }
        case EXPR_VAR: {
            PyObject* name = PyObject_GetAttrString(pyExpr, "name");
            if (!name || !PyUnicode_Check(name)) {
                fprintf(stderr, "Invalid variable expression\n");
                Py_XDECREF(name);
                return lean_box(0);
            }
            
            const char* var_name = PyUnicode_AsUTF8(name);
            printf("Creating Lean object for variable: %s\n", var_name);
            
            result = lean_alloc_ctor(1, 1, 0); // Constructor tag 1 for Var
            
            // Create a Lean Name object for the variable
            lean_object* str = lean_mk_string(var_name);
            lean_object* anonymous = lean_alloc_ctor(0, 0, 0); // Anonymous
            lean_object* lean_name = lean_alloc_ctor(1, 2, 0); // Str constructor for Name
            lean_ctor_set(lean_name, 0, anonymous);
            lean_ctor_set(lean_name, 1, str);
            
            lean_ctor_set(result, 0, lean_name);
            
            Py_XDECREF(name);
            break;
        }
        case EXPR_NEG: {
            PyObject* arg = PyObject_GetAttrString(pyExpr, "arg");
            if (!arg) {
                fprintf(stderr, "Invalid negation expression\n");
                Py_XDECREF(arg);
                return lean_box(0);
            }
            
            printf("Creating Lean object for negation\n");
            result = lean_alloc_ctor(2, 1, 0); // Constructor tag 2 for Neg
            lean_object* expr = traverse_expr(arg);
            lean_ctor_set(result, 0, expr);
            
            Py_XDECREF(arg);
            break;
        }
        case EXPR_ADD: {
            PyObject* left = PyObject_GetAttrString(pyExpr, "left");
            PyObject* right = PyObject_GetAttrString(pyExpr, "right");
            if (!left || !right) {
                fprintf(stderr, "Invalid addition expression\n");
                Py_XDECREF(left);
                Py_XDECREF(right);
                return lean_box(0);
            }
            
            printf("Creating Lean object for addition\n");
            result = lean_alloc_ctor(3, 2, 0); // Constructor tag 3 for Add
            lean_object* lean_left = traverse_expr(left);
            lean_object* lean_right = traverse_expr(right);
            lean_ctor_set(result, 0, lean_left);
            lean_ctor_set(result, 1, lean_right);
            
            Py_XDECREF(left);
            Py_XDECREF(right);
            break;
        }
        case EXPR_MATMULT: {
            PyObject* left = PyObject_GetAttrString(pyExpr, "left");
            PyObject* right = PyObject_GetAttrString(pyExpr, "right");
            if (!left || !right) {
                fprintf(stderr, "Invalid matrix multiplication expression\n");
                Py_XDECREF(left);
                Py_XDECREF(right);
                return lean_box(0);
            }
            
            printf("Creating Lean object for matrix multiplication\n");
            result = lean_alloc_ctor(4, 2, 0); // Constructor tag 4 for MatMult
            lean_object* lean_left = traverse_expr(left);
            lean_object* lean_right = traverse_expr(right);
            lean_ctor_set(result, 0, lean_left);
            lean_ctor_set(result, 1, lean_right);
            
            Py_XDECREF(left);
            Py_XDECREF(right);
            break;
        }
        case EXPR_TENSOR: {
            PyObject* dim1 = PyObject_GetAttrString(pyExpr, "dim1");
            PyObject* dim2 = PyObject_GetAttrString(pyExpr, "dim2");
            if (!dim1 || !dim2) {
                fprintf(stderr, "Invalid tensor expression\n");
                Py_XDECREF(dim1);
                Py_XDECREF(dim2);
                return lean_box(0);
            }
            
            printf("Creating Lean object for tensor type\n");
            result = lean_alloc_ctor(5, 2, 0); // Constructor tag 5 for Tensor
            lean_object* lean_dim1 = traverse_expr(dim1);
            lean_object* lean_dim2 = traverse_expr(dim2);
            lean_ctor_set(result, 0, lean_dim1);
            lean_ctor_set(result, 1, lean_dim2);
            
            Py_XDECREF(dim1);
            Py_XDECREF(dim2);
            break;
        }
        default:
            fprintf(stderr, "Unknown expression type: %d\n", expr_tag);
            return lean_box(0);
    }
    
    return result;
}

// Convert a Python statement to a Lean object
lean_object* traverse_stmt(PyObject* pyStmt) {
    if (!pyStmt || pyStmt == Py_None) {
        return lean_box(0);
    }
    
    PyObject* tag = PyObject_GetAttrString(pyStmt, "tag");
    if (!tag || !PyLong_Check(tag)) {
        fprintf(stderr, "Invalid statement tag\n");
        Py_XDECREF(tag);
        return lean_box(0);
    }
    
    int stmt_tag = PyLong_AsLong(tag);
    Py_XDECREF(tag);
    
    lean_object* result;
    
    switch (stmt_tag) {
        case STMT_ASSIGN: {
            PyObject* name = PyObject_GetAttrString(pyStmt, "name");
            PyObject* value = PyObject_GetAttrString(pyStmt, "value");
            if (!name || !PyUnicode_Check(name) || !value) {
                fprintf(stderr, "Invalid assignment statement\n");
                Py_XDECREF(name);
                Py_XDECREF(value);
                return lean_box(0);
            }
            
            const char* var_name = PyUnicode_AsUTF8(name);
            printf("Creating Lean object for assignment statement: %s\n", var_name);
            
            result = lean_alloc_ctor(0, 2, 0); // Constructor tag 0 for assign
            
            // Create a Lean Name object for the variable
            lean_object* str = lean_mk_string(var_name);
            lean_object* anonymous = lean_alloc_ctor(0, 0, 0); // Anonymous
            lean_object* lean_name = lean_alloc_ctor(1, 2, 0); // Str constructor for Name
            lean_ctor_set(lean_name, 0, anonymous);
            lean_ctor_set(lean_name, 1, str);
            
            lean_object* lean_value = traverse_expr(value);
            
            lean_ctor_set(result, 0, lean_name);
            lean_ctor_set(result, 1, lean_value);
            
            Py_XDECREF(name);
            Py_XDECREF(value);
            break;
        }
        case STMT_RETURN: {
            PyObject* value = PyObject_GetAttrString(pyStmt, "value");
            if (!value) {
                fprintf(stderr, "Invalid return statement\n");
                Py_XDECREF(value);
                return lean_box(0);
            }
            
            printf("Creating Lean object for return statement\n");
            result = lean_alloc_ctor(1, 1, 0); // Constructor tag 1 for return
            lean_object* lean_value = traverse_expr(value);
            lean_ctor_set(result, 0, lean_value);
            
            Py_XDECREF(value);
            break;
        }
        default:
            fprintf(stderr, "Unknown statement type: %d\n", stmt_tag);
            return lean_box(0);
    }
    
    return result;
}

// Convert a Python list of statements to a Lean list
lean_object* traverse_stmt_list(PyObject* pyStmtList) {
    if (!pyStmtList || !PyList_Check(pyStmtList)) {
        return lean_box(0);
    }
    
    Py_ssize_t size = PyList_Size(pyStmtList);
    
    // Create an empty array
    lean_object* array = lean_mk_empty_array();
    
    // Fill the array with traversed statements
    for (Py_ssize_t i = 0; i < size; i++) {
        PyObject* stmt = PyList_GetItem(pyStmtList, i);
        lean_object* lean_stmt = traverse_stmt(stmt);
        array = lean_array_push(array, lean_stmt);
    }
    
    // Convert array to list
    lean_object* list = lean_array_to_list(array);
    
    return list;
}

// Main traverse_function implementation
lean_object* lean_traverse_function(PyObject* pyFunction) {
    if (!pyFunction || pyFunction == Py_None) {
        return lean_box(0);
    }
    
    printf("Creating Lean object for Py.FunDef.funDef with constructor tag 0\n");
    
    // Get function name
    PyObject* name = PyObject_GetAttrString(pyFunction, "name");
    if (!name || !PyUnicode_Check(name)) {
        fprintf(stderr, "Function has no name\n");
        Py_XDECREF(name);
        return lean_box(0);
    }
    
    // Get function body
    PyObject* body = PyObject_GetAttrString(pyFunction, "body");
    if (!body || !PyList_Check(body)) {
        fprintf(stderr, "Function has no body\n");
        Py_XDECREF(name);
        Py_XDECREF(body);
        return lean_box(0);
    }
    
    // Get function annotations
    PyObject* annotations = PyObject_GetAttrString(pyFunction, "annotations");
    if (!annotations || !PyDict_Check(annotations)) {
        fprintf(stderr, "Function has no annotations\n");
        Py_XDECREF(name);
        Py_XDECREF(body);
        Py_XDECREF(annotations);
        return lean_box(0);
    }
    
    // Create Lean function object
    lean_object* result = lean_alloc_ctor(0, 4, 0);
    
    // Set the name
    lean_object* lean_name = lean_mk_string(PyUnicode_AsUTF8(name));
    lean_ctor_set(result, 0, lean_name);
    
    // Set the body
    lean_object* lean_body = traverse_stmt_list(body);
    lean_ctor_set(result, 1, lean_body);
    
    // Get input annotations
    PyObject* input_a = PyDict_GetItemString(annotations, "input_a");
    PyObject* input_b = PyDict_GetItemString(annotations, "input_b");
    char combined_input[256] = "";
    
    if (input_a && PyUnicode_Check(input_a)) {
        strcat(combined_input, PyUnicode_AsUTF8(input_a));
    }
    
    if (input_b && PyUnicode_Check(input_b)) {
        if (strlen(combined_input) > 0) {
            strcat(combined_input, ", ");
        }
        strcat(combined_input, PyUnicode_AsUTF8(input_b));
    }
    
    lean_object* lean_input_ann = lean_mk_string(combined_input);
    lean_ctor_set(result, 2, lean_input_ann);
    
    // Get output annotation
    PyObject* output = PyDict_GetItemString(annotations, "output");
    const char* output_str = (output && PyUnicode_Check(output)) ? PyUnicode_AsUTF8(output) : "";
    lean_object* lean_output_ann = lean_mk_string(output_str);
    lean_ctor_set(result, 3, lean_output_ann);
    
    // Debug print the fields
    printf("Lean object fields:\n");
    printf("  Name: %s\n", PyUnicode_AsUTF8(name));
    printf("  Input annotations: %s\n", combined_input);
    printf("  Output annotation: %s\n", output_str);
    
    // Clean up Python objects
    Py_XDECREF(name);
    Py_XDECREF(body);
    Py_XDECREF(annotations);
    
    return result;
}

// ==========================================================================
// Lean Entry Point
// ==========================================================================

// Function called from Lean
lean_object* c_process_function(lean_object* unit) {
    printf("DEBUG: Starting process_function\n");
    FunctionOpt o = proc_function();
    
    if (!o.safe) {
        printf("Unsafe function\n");
        lean_object* none = lean_box(0);
        lean_inc_ref(none);
        return none;
    } else {
        printf("Safe function\n");
        lean_object* ob = lean_alloc_ctor(1, 1, 8);
        lean_inc_ref(ob);
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
        
        // Clean up Python objects
        Py_DECREF((PyObject*)o.ptr);
        
        return ob;
    }
}
