import ShapeChecker.Backend.Basic
import ShapeChecker.Backend.Tensors

def ElementwiseCompatible (a b : Py.Shape) : Prop := a = b

-- l ++ [a, n], l ++ [n, b]
inductive MatMulRel : Py.Shape -> Py.Shape -> Prop :=
  | generalVar : MatMulRel
    (.append l $ .cons _ $ .cons (.var n) $ .nil)
    (.append l $ .cons (.var n) $ .cons _ $ .nil)
  | generalConst : MatMulRel
    (.append l $ .cons _ $ .cons (.const n) $ .nil)
    (.append l $ .cons (.const n) $ .cons _ $ .nil)

namespace PrimOp

def add : Tensor a -> Tensor b -> ElementwiseCompatible a b -> Tensor a := λ _ _ _ => .mk

def matmul :
  Tensor (.append l $ .cons m $ .cons n $ .nil) ->
  Tensor (.append l $ .cons n $ .cons d $ .nil) ->
  MatMulRel
    (.append l $ .cons m $ .cons n $ .nil)
    (.append l $ .cons n $ .cons d $ .nil) ->
  Tensor (.append l $ .cons m $ .cons d $ .nil) := λ _ _ _ => .mk

end PrimOp
