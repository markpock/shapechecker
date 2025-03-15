import ShapeChecker.Frontend.PythonAdapter

@[match_pattern] def Tree.singleton (data : Int) : Tree := .branch data .nil .nil

def Tree.toString : Tree -> String
  | .nil => "·"
  | .singleton data => s!"{data}"
  | .branch data left right =>
    let x := left.toString
    let y := toString right
    s!"{data}, \{{x}} \{{y}}"

instance : ToString (Tree) := ⟨Tree.toString⟩

def Expr.toString : Expr → String
  | .int n => s!"{n}"
  | .var v => v.toString
  | .neg arg => s!"-{arg.toString}"
  | .add l r => s!"({l.toString} + {r.toString})"

instance : ToString Expr := ⟨Expr.toString⟩

-- Replace the existing main function
def main : IO Unit := do
  .println "Hello World"
  -- Test Tree
  let tree := ct ()
  match tree with
  | .none => IO.println "Tree: Failed to get tree from Python"
  | .some t => IO.println s!"Tree: {t}"
  .println ""

  let expr : Option Expr := process_expr ()
  .println "Expression"
  match expr with
  | .none => IO.println "Expr: Failed to get expression from Python"
  | .some (.int e) => IO.println s!"Expr-int: {e}"
  | .some (.var v) => IO.println s!"Expr-var: {v}"
  | .some (.neg e) => IO.println s!"Expr-neg: {e.toString}"
  | .some (.add l r) => IO.println s!"Expr-add: {l.toString} + {r.toString}"

-- #eval! (ct (1 : UInt32))
-- #eval! (process_expr () )
