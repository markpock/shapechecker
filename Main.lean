import ShapeChecker.Frontend.PythonAdapter

inductive Tree where
  | nil : Tree
  | branch (data : Int) (left right : Tree) : Tree
  deriving Repr

@[match_pattern] def Tree.singleton (data : Int) : Tree := .branch data .nil .nil

def Tree.toString : Tree -> String
  | .nil => "·"
  | .singleton data => s!"{data}"
  | .branch data left right =>
    let x := left.toString
    let y := toString right
    s!"{data}, \{{x}} \{{y}}"

instance : ToString Tree := ⟨Tree.toString⟩
instance : ToString (Tree) := ⟨Tree.toString⟩

#check Expr
-- inductive Expr where
--   | int : Int → Expr
--   | float : Float → Expr
--   | neg : Expr → Expr
--   | var : String → Expr
-- deriving Repr

-- Expression toString handler
def Expr.toString : Expr → String
  | .int n => n.repr
  | .var v => v.toString
  | .neg e => s!"-{toString e}"
  | .add e1 e2 => s!"({toString e1} + {toString e2})"
  | .matMult e1 e2 => s!"({toString e1} @ {toString e2})"
  | .tensor e1 e2 => s!"Tensor({toString e1}, {toString e2})"

instance : ToString Expr := ⟨Expr.toString⟩

-- Statement toString handler
def Stmt.toString : Stmt → String
  | .assign name expr => s!"{name} = {expr.toString}"
  | .ret expr => s!"return {expr.toString}"

instance : ToString Stmt := ⟨Stmt.toString⟩

-- Function definition toString handler
instance : ToString Py.FunDef where
  toString
    | ⟨name, body, inputAnnotations, outputAnnotation⟩ =>
      let bodyStr := String.intercalate "\n  " (body.map toString)
      s!"Function: {name}\nInput annotations: {inputAnnotations}\nOutput annotation: {outputAnnotation}\nBody:\n  {bodyStr}"

def main : IO Unit := do
  IO.println "Starting main function"
  let opt := process_function ()
  IO.println "Process function completed"
  match opt with
  | none =>
    IO.println "Failed to process function"
  | some funDef =>
    IO.println "Successfully processed function:"
    IO.println (toString funDef)

-- #eval! (ct (1 : UInt32))
-- #eval! (process_expr () )
