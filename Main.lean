import ShapeChecker.Basic
import ShapeChecker.Backend.Elaborator
import ShapeChecker.Frontend.PythonAdapter

def main : IO Unit := do
  let x := ct ()
  .println "Hello"
  match x with
  | .none => .println "Unable to parse tree from Python"
  | .some t => .println t.toString
