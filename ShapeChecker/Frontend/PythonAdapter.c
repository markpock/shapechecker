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
    Py_Initialize();

    PyObject* pModule1 = PyImport_Import(PyUnicode_FromString("sys"));

    PyRun_SimpleString("import sys; sys.path.append('./examples/python-scripts/')");
    PyObject* pName = PyUnicode_FromString("simpleTree");
    if (pName == NULL) {
        PyErr_Print();
        return None;
    }

    PyObject* pModule = PyImport_Import(pName);
    Py_DECREF(pName);

    if (pModule == NULL) {
        PyErr_Print();
        return None;
    }

    PyObject* pFunc = PyObject_GetAttrString(pModule, "simpleTree");
    /* pFunc is a new reference */
    if (!pFunc || !PyCallable_Check(pFunc)) {
        if (PyErr_Occurred()) PyErr_Print();

        Py_XDECREF(pFunc);
        Py_DECREF(pModule);
        Py_Finalize();
        return None;
    }

    PyObject* pValue = PyObject_CallObject(pFunc, NULL);
    if (pValue == NULL) {
        PyErr_Print();
        fprintf(stderr, "Call failed\n");
        return None;
    }

    Opt o = treeToC (pValue);
    if (!o.safe) return None;
    BTNode* node = o.ptr;
    printTree(node); printf("\n");

    return Some(node);
}
