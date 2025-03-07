import ShapeChecker.Backend.Basic
import ShapeChecker.Backend.Elaborator

/-
def exa(a : int, b : int) -> int:
  x = a
  y = x + 1
  return x
-/
def exampleFunction : Py.FunDef := {
  name   := `exa
  params := [
    (`a, .float),
    (`b, .float)
  ]
  body   := [
    .assign `x $ .var `a,
    .assign `y $ .add (.var `x) (.int 1),
    .for `z (.list [.int 1, .int 2, .int 3]) [] (.assign `x $ .add (.var `x) (.var `z))
  ]
  retval := .var `x
  rettyp := .float
}

#pyFunc exampleFunction


-- def exa : Float -> Float -> Int := λ a b => Id.run do
--   let x := a
--   let y := (x + 1)
--   for z in List.cons 1 (List.cons 2 (List.cons 3 (List.nil)))do
--     let x := (x + z)
--   return x

/-
def tensorAddN(a : Tensor (l), b : Tensor (l)) -> Tensor (l):
  return a + b
-/
def tensorAdd : Py.FunDef := {
  name   := `tensorAddN
  params := [
    (`a, .tensor (.var `l)),
    (`b, .tensor (.var `l))
  ]
  body   := []
  retval := .add (.var `a) (.var `b)
  rettyp := .tensor (.var `l)
}

def tensorAddAltered : Py.FunDef := {
  name   := `tensorAddAlteration
  params := [
    (`a, .tensor (.var `l)),
    (`b, .tensor (.var `l))
  ]
  body   := []
  retval := .call (.var `tensorAddPrimOp) [.var `a, .var `b]
  rettyp := .tensor (.var `l)
}

/-
def tensorMatMul(
  a : Tensor ([2] + [3, 4]),
  b : Tensor ([2] + [4, 5])
) -> Tensor ([2] + [3, 5]):
  c = a + b
  d = c + a
  return a @ d
-/
def tensorMatMulAltered : Py.FunDef := {
  name   := `tensorMatMulAlteration
  params := [
    (`a, .tensor (.append (.cons 2 .nil) (.cons 3 $ .cons 4 .nil))),
    (`b, .tensor (.append (.cons 2 .nil) (.cons 4 $ .cons 5 .nil)))
  ]
  body   := [
    Py.Stmt.assign `c (Py.Expr.add (Py.Expr.var `a) (Py.Expr.var `b))
  ]
  retval := .call (.var `tensorMatMulPrimOp) [.var `a, .var `b]
  rettyp := .tensor (.append (.cons 2 .nil) (.cons 3 $ .cons 5 .nil))
}
