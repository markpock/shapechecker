#include <Python.h>
#include <stdbool.h>

// Tree structures
typedef struct binary_tree_node_s
{
    int data;
    struct binary_tree_node_s *left, *right;
} BTNode;

typedef struct opt
{
    bool safe;
    BTNode *ptr;
} Opt;

// Expr structures
typedef struct expr_node_s
{
    int integer;
    double float_val;
    struct expr_node_s *neg;
    char *var;
    // Additional fields for other expr types can be added here
    struct expr_node_s *left;  // For add and matmul
    struct expr_node_s *right; // For add and matmul
    int expr_type;             // To track which type of expr this is
    char *fn_name;             // For function calls
    struct expr_node_s **args; // For function call arguments
    int arg_count;             // Number of arguments
} ExprNode;

#define EXPR_INT 0
#define EXPR_FLOAT 1
#define EXPR_NEG 2
#define EXPR_VAR 3

#define None ((Opt){false, NULL})
#define Some(x) ((Opt){true, x})

typedef struct expr_opt
{
    bool safe;
    ExprNode *ptr;
} ExprOpt;

#define None ((Opt){false, NULL})
#define Some(x) ((Opt){true, x})
#define ExprNone ((ExprOpt){false, NULL})
#define ExprSome(x) ((ExprOpt){true, x})

Opt treeToC(PyObject *);
void printTree(BTNode *);
Opt proc();

// Assume that PyObject is non-null.
Opt treeToC(PyObject *value)
{
    if (value == NULL)
    {
        return None;
    }
    if (Py_Is(value, Py_None))
    {
        return Some(NULL);
    }
    PyObject *data = PyObject_GetAttrString(value, "data");
    if (!data || !PyLong_Check(data))
    {
        fprintf(stderr, "Invalid datum\n");
        return None;
    }
    PyObject *left = PyObject_GetAttrString(value, "left");
    PyObject *right = PyObject_GetAttrString(value, "right");
    BTNode *result = malloc(sizeof(BTNode));
    Opt ol = treeToC(left);
    Opt or = treeToC(right);
    if (!ol.safe)
        return None;
    if (!or.safe)
        return None;
    *result = (BTNode){PyLong_AsLong(data), ol.ptr, or.ptr};
    Py_XDECREF(data);
    Py_XDECREF(left);
    Py_XDECREF(right);
    return Some(result);
}

// Add this function before using it
char *strdup_safe(const char *str)
{
    if (str == NULL)
        return NULL;
    char *result = strdup(str);
    if (result == NULL)
    {
        fprintf(stderr, "Memory allocation failed in strdup_safe\n");
        exit(1); // Or handle error differently
    }
    return result;
}

// Simplified exprToC implementation
ExprOpt exprToC(PyObject *value)
{
    if (value == NULL)
    {
        return ExprNone;
    }
    if (Py_Is(value, Py_None))
    {
        return ExprSome(NULL);
    }

    // Allocate a new expression node
    ExprNode *result = malloc(sizeof(ExprNode));
    if (result == NULL)
    {
        fprintf(stderr, "Memory allocation failed\n");
        return ExprNone;
    }

    // Initialize all fields to zero/NULL
    result->integer = 0;
    result->float_val = 0.0;
    result->neg = NULL;
    result->var = NULL;
    result->expr_type = EXPR_INT; // Default type

    // Try to get integer field
    PyObject *integer = PyObject_GetAttrString(value, "integer");
    if (integer && PyLong_Check(integer))
    {
        result->integer = PyLong_AsLong(integer);
        result->expr_type = EXPR_INT;
    }
    Py_XDECREF(integer);

    // Try to get float field
    PyObject *float_val = PyObject_GetAttrString(value, "float");
    if (float_val && PyFloat_Check(float_val))
    {
        result->float_val = PyFloat_AsDouble(float_val);
        result->expr_type = EXPR_FLOAT;
    }
    Py_XDECREF(float_val);

    // Try to get var field
    PyObject *var = PyObject_GetAttrString(value, "var");
    if (var && PyUnicode_Check(var))
    {
        const char *var_str = PyUnicode_AsUTF8(var);
        if (var_str && strlen(var_str) > 0)
        {
            result->var = strdup_safe(var_str);
            result->expr_type = EXPR_VAR;
        }
    }
    Py_XDECREF(var);

    // Try to get neg field
    PyObject *neg = PyObject_GetAttrString(value, "neg");
    if (neg && !Py_Is(neg, Py_None))
    {
        ExprOpt neg_opt = exprToC(neg);
        if (neg_opt.safe)
        {
            result->neg = neg_opt.ptr;
            result->expr_type = EXPR_NEG;
        }
        else
        {
            // If we can't convert the negation, free the current node and return error
            free(result);
            Py_XDECREF(neg);
            return ExprNone;
        }
    }
    Py_XDECREF(neg);

    return ExprSome(result);
}

void printTree(BTNode *node)
{
    if (node == NULL)
    {
        printf("*");
        return;
    }
    printf("%d; ( ", node->data);
    printTree(node->left);
    printf(" ); ( ");
    printTree(node->right);
    printf(" )");
}

void printExpr(ExprNode *expr)
{
    if (expr == NULL)
    {
        printf("NULL");
        return;
    }

    switch (expr->expr_type)
    {
    case EXPR_INT:
        printf("Int(%d)", expr->integer);
        break;

    case EXPR_FLOAT:
        printf("Float(%f)", expr->float_val);
        break;

    case EXPR_VAR:
        printf("Var(%s)", expr->var ? expr->var : "NULL");
        break;

    case EXPR_NEG:
        printf("Neg(");
        printExpr(expr->neg);
        printf(")");
        break;

    default:
        printf("Unknown expression type");
    }
}

Opt proc()
{
    printf("Entering proc\n");
    Py_Initialize();

    PyObject *pModule1 = PyImport_Import(PyUnicode_FromString("sys"));
    // PyObject res1 = PyRun_SimpleString("'Hello World'");

    PyRun_SimpleString("import sys; print(sys.argv[0]); sys.path.append('./Backend')");
    PyObject *pName = PyUnicode_FromString("script");
    /* Error checking of pName left out */

    PyObject *pModule = PyImport_Import(pName);
    Py_DECREF(pName);

    if (pModule == NULL)
    {
        PyErr_Print();
        printf("RET1\n");
        return None;
    }

    PyObject *pFunc = PyObject_GetAttrString(pModule, "greet");
    /* pFunc is a new reference */
    if (!pFunc || !PyCallable_Check(pFunc))
    {
        if (PyErr_Occurred())
            PyErr_Print();

        Py_XDECREF(pFunc);
        Py_DECREF(pModule);
        Py_Finalize();

        printf("RET2\n");
        return None;
    }

    PyObject *pValue = PyObject_CallObject(pFunc, NULL);
    if (pValue == NULL)
    {
        fprintf(stderr, "Call failed\n");
        printf("RET3\n");
        return None;
    }

    Opt o = treeToC(pValue);
    if (!o.safe)
        return None;
    BTNode *node = o.ptr;
    printTree(node);

    printf("RET4\n");
    return Some(node);
}

ExprOpt proc_expr()
{
    printf("Entering proc_expr\n");
    Py_Initialize();
    PyObject *pModule1 = PyImport_Import(PyUnicode_FromString("sys"));
    PyRun_SimpleString("import sys; print(sys.argv[0]); sys.path.append('./Backend')");
    PyObject *pName = PyUnicode_FromString("script");

    PyObject *pModule = PyImport_Import(pName);
    Py_DECREF(pName);
    if (pModule == NULL)
    {
        PyErr_Print();
        printf("Failed to import script module\n");
        return ExprNone;
    }

    PyObject *pFunc = PyObject_GetAttrString(pModule, "greet2");
    if (!pFunc || !PyCallable_Check(pFunc))
    {
        if (PyErr_Occurred())
            PyErr_Print();
        Py_XDECREF(pFunc);
        Py_DECREF(pModule);
        Py_Finalize();
        printf("Failed to find greet2 function\n");
        return ExprNone;
    }

    PyObject *pValue = PyObject_CallObject(pFunc, NULL);
    if (pValue == NULL)
    {
        fprintf(stderr, "Call to greet2 failed\n");
        return ExprNone;
    }

    ExprOpt o = exprToC(pValue);
    if (!o.safe)
        return ExprNone;

    ExprNode *expr = o.ptr;
    printf("Expression from Python: ");
    printExpr(expr);
    printf("\n");
    return ExprSome(expr);
}

int hw() { return 1; }
Opt hw2() { return None; }
