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

// Expression structure using tagged union
typedef struct taggedUExpr
{
    int tag;
    union uExpr
    {
        int i;
        char *var;
        struct taggedUExpr *neg;
        struct eAdd
        {
            struct taggedUExpr *left;
            struct taggedUExpr *right;
        } a;
    } u;
} TaggedExpr;

#define EXPR_INT 0
#define EXPR_VAR 1
#define EXPR_NEG 2
#define EXPR_ADD 3

typedef struct expr_opt
{
    bool safe;
    TaggedExpr *ptr;
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
    TaggedExpr *result = malloc(sizeof(TaggedExpr));
    if (result == NULL)
    {
        fprintf(stderr, "Memory allocation failed\n");
        return ExprNone;
    }

    // Try to get integer field
    PyObject *integer = PyObject_GetAttrString(value, "integer");
    if (integer && PyLong_Check(integer))
    {
        result->tag = EXPR_INT;
        result->u.i = PyLong_AsLong(integer);
        Py_XDECREF(integer);
        return ExprSome(result);
    }
    Py_XDECREF(integer);

    // Try to get var field
    PyObject *var = PyObject_GetAttrString(value, "var");
    if (var && PyUnicode_Check(var))
    {
        const char *var_str = PyUnicode_AsUTF8(var);
        printf("DEBUG: Found var field with value: %s\n", var_str ? var_str : "NULL");
        if (var_str && strlen(var_str) > 0)
        {
            result->tag = EXPR_VAR;
            result->u.var = strdup_safe(var_str);
            printf("DEBUG: Created variable expression with tag %d and value %s\n", result->tag, result->u.var);
            Py_XDECREF(var);
            return ExprSome(result);
        }
    }
    Py_XDECREF(var);

    // Try to get neg field
    PyObject *neg = PyObject_GetAttrString(value, "neg");
    if (neg && !Py_Is(neg, Py_None))
    {
        printf("DEBUG: Found neg field\n");
        ExprOpt neg_opt = exprToC(neg);
        if (neg_opt.safe)
        {
            result->tag = EXPR_NEG;
            result->u.neg = neg_opt.ptr;
            printf("DEBUG: Created negation expression with tag %d\n", result->tag);
            Py_XDECREF(neg);
            return ExprSome(result);
        }
        free(result);
        Py_XDECREF(neg);
        return ExprNone;
    }
    Py_XDECREF(neg);

    // Try to get left and right fields for addition
    PyObject *left = PyObject_GetAttrString(value, "left");
    PyObject *right = PyObject_GetAttrString(value, "right");
    if (left && right && !Py_Is(left, Py_None) && !Py_Is(right, Py_None))
    {
        printf("DEBUG: Found left and right fields for addition\n");
        ExprOpt left_opt = exprToC(left);
        if (!left_opt.safe)
        {
            free(result);
            Py_XDECREF(left);
            Py_XDECREF(right);
            return ExprNone;
        }

        ExprOpt right_opt = exprToC(right);
        if (!right_opt.safe)
        {
            free(result);
            Py_XDECREF(left);
            Py_XDECREF(right);
            return ExprNone;
        }

        result->tag = EXPR_ADD;
        result->u.a.left = left_opt.ptr;
        result->u.a.right = right_opt.ptr;
        printf("DEBUG: Created addition expression with tag %d\n", result->tag);
        Py_XDECREF(left);
        Py_XDECREF(right);
        return ExprSome(result);
    }
    Py_XDECREF(left);
    Py_XDECREF(right);

    free(result);
    return ExprNone;
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

void printExpr(TaggedExpr *expr)
{
    if (expr == NULL)
    {
        printf("NULL");
        return;
    }

    switch (expr->tag)
    {
    case EXPR_INT:
        printf("Int(%d)", expr->u.i);
        break;
    case EXPR_VAR:
        printf("Var(%s)", expr->u.var ? expr->u.var : "NULL");
        break;
    case EXPR_NEG:
        printf("Neg(");
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

    PyRun_SimpleString("import sys; print(sys.argv[0]); sys.path.append('./examples/python-scripts');");

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
    PyRun_SimpleString("import sys; print(sys.argv[0]); sys.path.append('./examples/python-scripts')");
    PyObject *pName = PyUnicode_FromString("script");

    PyObject *pModule = PyImport_Import(pName);
    Py_DECREF(pName);
    if (pModule == NULL)
    {
        PyErr_Print();
        printf("Failed to import script module\n");
        return ExprNone;
    }

    // First try int expression
    // PyObject *pFunc = PyObject_GetAttrString(pModule, "make_int_expr");
    // if (pFunc && PyCallable_Check(pFunc))
    // {
    //     PyObject *pValue = PyObject_CallObject(pFunc, NULL);
    //     if (pValue != NULL)
    //     {
    //         ExprOpt o = exprToC(pValue);
    //         if (o.safe)
    //         {
    //             TaggedExpr *expr = o.ptr;
    //             printf("Expression from Python (int): ");
    //             printExpr(expr);
    //             printf("\n");
    //             return ExprSome(expr);
    //         }
    //     }
    // }
    // if (PyErr_Occurred())
    //     PyErr_Clear();
    // Py_XDECREF(pFunc);

    // Then try var expression
    // PyObject *pFunc = PyObject_GetAttrString(pModule, "make_var_expr");
    // if (pFunc && PyCallable_Check(pFunc))
    // {
    //     PyObject *pValue = PyObject_CallObject(pFunc, NULL);
    //     if (pValue != NULL)
    //     {
    //         ExprOpt o = exprToC(pValue);
    //         if (o.safe)
    //         {
    //             TaggedExpr *expr = o.ptr;
    //             printf("Expression from Python (var): ");
    //             printExpr(expr);
    //             printf("\n");
    //             return ExprSome(expr);
    //         }
    //     }
    // }
    // if (PyErr_Occurred())
    // {
    //     PyErr_Print();
    // }
    // Py_XDECREF(pFunc);
    // Py_DECREF(pModule);

    // Then try neg expression
    // PyObject *pFunc2 = PyObject_GetAttrString(pModule, "make_neg_expr");
    // if (pFunc2 && PyCallable_Check(pFunc2))
    // {
    //     PyObject *pValue = PyObject_CallObject(pFunc2, NULL);
    //     if (pValue != NULL)
    //     {
    //         ExprOpt o = exprToC(pValue);
    //         if (o.safe)
    //         {
    //             TaggedExpr *expr = o.ptr;
    //             printf("Expression from Python (neg): ");
    //             printExpr(expr);
    //             printf("\n");
    //             return ExprSome(expr);
    //         }
    //     }
    // }
    // if (PyErr_Occurred())
    // {
    //     PyErr_Print();
    // }
    // Py_XDECREF(pFunc2);
    // Py_DECREF(pModule);

    // Then try add expression
    PyObject *pFunc3 = PyObject_GetAttrString(pModule, "make_add_expr");
    if (pFunc3 && PyCallable_Check(pFunc3))
    {
        PyObject *pValue = PyObject_CallObject(pFunc3, NULL);
        if (pValue != NULL)
        {
            ExprOpt o = exprToC(pValue);
            if (o.safe)
            {
                TaggedExpr *expr = o.ptr;
                printf("Expression from Python (add): ");
                printExpr(expr);
                printf("\n");
                return ExprSome(expr);
            }
        }
    }
    if (PyErr_Occurred())
    {
        PyErr_Print();
    }
    Py_XDECREF(pFunc3);
    Py_DECREF(pModule);

    Py_Finalize();
    printf("Both expressions failed\n");
    return ExprNone;
}

int hw() { return 1; }
Opt hw2() { return None; }
