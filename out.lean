import ShapeChecker
def matrix_multiply :
    Tensor (Py.Shape.append (Py.Shape.lift 8) (Py.Shape.append (Py.Shape.lift 4) (Py.Shape.nil))) ->
      Tensor (Py.Shape.append (Py.Shape.lift 4) (Py.Shape.append (Py.Shape.lift 5) (Py.Shape.nil))) ->
        Tensor (Py.Shape.append (Py.Shape.lift 7) (Py.Shape.append (Py.Shape.lift 100) (Py.Shape.nil))) ->
          Tensor (Py.Shape.append (Py.Shape.lift 8) (Py.Shape.append (Py.Shape.lift 5) (Py.Shape.nil))) :=
  λ a b c =>
  Id.run
    (do
      let mut d := (a @b)
      let mut c := (a + b)
      return ((a @b) + n))