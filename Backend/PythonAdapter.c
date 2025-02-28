#include <Python.h>
#include <stdbool.h>

typedef struct binary_tree_node_s {
    int data;
    struct binary_tree_node_s *left, *right;
} BTNode;

typedef struct opt {
    bool safe;
    BTNode* ptr;
} Opt;

#define None ((Opt){false, NULL})
#define Some(x) ((Opt){true, x})


Opt treeToC(PyObject*);
void printTree(BTNode*);
Opt proc();

// Assume that PyObject is non-null.
Opt treeToC (PyObject* value) {
    if (value == NULL) {
        return None;
    }
    if (Py_Is(value, Py_None)) {
        return Some(NULL);
    }
    PyObject* data = PyObject_GetAttrString(value, "data");
    if (!data || !PyLong_Check(data)) {
        fprintf(stderr, "Invalid datum\n");
        return None;
    }
    PyObject* left = PyObject_GetAttrString(value, "left");
    PyObject* right = PyObject_GetAttrString(value, "right");
    BTNode* result = malloc(sizeof(BTNode));
    Opt ol = treeToC(left);
    Opt or = treeToC(right);
    if (!ol.safe) return None;
    if (!or.safe) return None;
    *result = (BTNode){PyLong_AsLong(data), ol.ptr, or.ptr};
    Py_XDECREF(data);
    Py_XDECREF(left);
    Py_XDECREF(right);
    return Some(result);
}

void printTree (BTNode* node) {
    if (node == NULL) { printf("*"); return; }
    printf("%d; ( ", node->data);
    printTree(node->left);
    printf(" ); ( ");
    printTree(node->right);
    printf(" )");
}

Opt proc() {
    printf("Entering proc\n");
    Py_Initialize();

    PyObject* pModule1 = PyImport_Import(PyUnicode_FromString("sys"));
    // PyObject res1 = PyRun_SimpleString("'Hello World'");

    PyRun_SimpleString("import sys; print(sys.argv[0]); sys.path.append('./Backend')");
    PyObject* pName = PyUnicode_FromString("script");
    /* Error checking of pName left out */

    PyObject* pModule = PyImport_Import(pName);
    Py_DECREF(pName);

    if (pModule == NULL) {
        PyErr_Print();
        printf("RET1\n");
        return None;
    }

    PyObject* pFunc = PyObject_GetAttrString(pModule, "greet");
    /* pFunc is a new reference */
    if (!pFunc || !PyCallable_Check(pFunc)) {
        if (PyErr_Occurred()) PyErr_Print();

        Py_XDECREF(pFunc);
        Py_DECREF(pModule);
        Py_Finalize();

        printf("RET2\n");
        return None;
    }

    PyObject* pValue = PyObject_CallObject(pFunc, NULL);
    if (pValue == NULL) {
        fprintf(stderr, "Call failed\n");
        printf("RET3\n");
        return None;
    }

    Opt o = treeToC (pValue);
    if (!o.safe) return None;
    BTNode* node = o.ptr;
    printTree(node);

    printf("RET4\n");
    return Some(node);
}

int hw() { return 1; }
Opt hw2() { return None; }

