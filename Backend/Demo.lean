import Backend.DemoBase

/-
def exa(a : int, b : int) -> int:
  x = a
  y = x + 1
  return x
-/
def exampleFunction : Py.FunDef := {
  name := `exa
  params := [
    (`a, .float),
    (`b, .float)
  ]
  body := [
    .assign `x $ .var `a,
    .assign `y $ .add (.var `x) (.int 1)
  ]
  retval := .var `x
  rettyp := .int
}

/-
def tensorAddN(a : Tensor (l), b : Tensor (l)) -> Tensor (l):
  return a + b
-/
def tensorAdd : Py.FunDef := {
  name := `tensorAddN
  params := [
    (`a, .tensor (.var `l)),
    (`b, .tensor (.var `l))
  ]
  body := []
  retval := .add (.var `a) (.var `b)
  rettyp := .tensor (.var `l)
}

def tensorAddAltered : Py.FunDef := {
  name := `tensorAddAlteration
  params := [
    (`a, .tensor (.var `l)),
    (`b, .tensor (.var `l))
  ]
  body := []
  retval := .call (.var `tensorAddPrimOp) [(.var `a), (.var `b)]
  rettyp := .tensor (.var `l)
}

/-
def tensorMatMul(
  a : Tensor ([2] + [3, 4]),
  b : Tensor ([2] + [4, 5])
) -> Tensor ([2] + [3, 5]):
  return a @ b
-/
def tensorMatMulAltered : Py.FunDef := {
  name := `tensorMatMulAlteration
  params := [
    (`a, .tensor ((2 ::: ∅) ++ (3 ::: 4 ::: ∅))),
    (`b, .tensor ((2 ::: ∅) ++ (4 ::: 5 ::: ∅)))
  ]
  body := []
  retval := .call (.var `tensorMatMulPrimOp) [(.var `a), (.var `b)]
  rettyp := .tensor ((2 ::: ∅) ++ (3 ::: 5 ::: ∅))
}

#pyFunc tensorMatMulAltered
