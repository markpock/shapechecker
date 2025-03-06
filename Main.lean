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

inductive Expr where
  | int : Int → Expr
  | float : Float → Expr
  | neg : Expr → Expr
  | var : String → Expr
deriving Repr

def Expr.toString : Expr → String
  | .int n => s!"{n}"
  | .float f => s!"{f}"
  | .neg arg => s!"-{arg.toString}"
  | .var v => s!"{v}"

instance : ToString (Expr) := ⟨Expr.toString⟩

-- Replace the existing main function
def main : IO Unit := do
  -- Test Tree
  let tree := ct ()
  match tree with
  | .none => IO.println "Tree: Failed to get tree from Python"
  | .some t => IO.println s!"Tree: {t}"

  -- Test Expr
  let expr := process_expr ()
  match expr with
  | .none => IO.println "Expr: Failed to get expression from Python"
  | .some e => IO.println s!"Expr: {e}"

-- #eval! (ct (1 : UInt32))
