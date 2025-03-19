/-
```c
struct eInt {
  int i;
}

struct eVar {
  char* name;
}


char* IGotThisFromTheAst p = ...
int length = ...
struct eVar x = // You copy over from AST
  malloc(length * sizeof (int));

memcpy(p, x, length); // Copies length bytes from address p to address x

struct eNeg {
  (struct taggedUExpr)* argument;
}





struct taggedUExpr {
  int tag;
  union uExpr {
    int i;
    char* var;
    (struct taggedUExpr)* neg;
    struct eAdd {
        (struct taggedUExpr)* left;
        (struct taggedUExpr)* right;
    } a;
  } u;
};

struct taggedUExpr e;
int x;
x = 1;



```

matmul :
  (a : Tensor A) [4, 3]
  (b : Tensor B) [3, 5]

  (l : Py.Shape)
  (m n d : Py.ShapeData)
  (pf1 : A ≃ l ++ (m, n))
  (pf2 : B ≃ l ++ (n, d))

  (C : Py.Shape)

  (pf3 : C ≃ l ++ (m, d))3

  :

  Tensor C

  nil ++ [4, 5]
  [4, 5]

-/
