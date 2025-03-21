import ShapeChecker.Frontend.PythonAdapter
import ShapeChecker.Backend.AST
import ShapeChecker.Backend.Elaborator
import Parser
import Lean

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

namespace Adapter

def Expr.toString : Expr → String
  | .int n => n.repr
  | .var v => v.toString
  | .neg e => s!"-{toString e}"
  | .add e1 e2 => s!"({toString e1} + {toString e2})"
  | .matMult e1 e2 => s!"({toString e1} @ {toString e2})"
  | .tensor e1 e2 => s!"Tensor({toString e1}, {toString e2})"
instance : ToString Expr := ⟨Expr.toString⟩

def Stmt.toString : Stmt → String
  | .assign name expr => s!"{name} = {expr.toString}"
  | .ret expr => s!"return {expr.toString}"

instance : ToString Stmt := ⟨Stmt.toString⟩
instance : ToString Adapter.FunDef where toString
  | ⟨name, body, inputAnnotations, outputAnnotation⟩ =>
    let bodyStr := String.intercalate "\n  " $ body.map toString
    s!"Function: {name}\nInput annotations: {inputAnnotations}\nOutput annotation: {outputAnnotation}\nBody:\n  {bodyStr}"

def Expr.toPyExpr (p : Expr) : Except String Py.Expr :=  do
  match p with
  | .add a b => .add <$> toPyExpr a <*> toPyExpr b
  | .tensor (.int $ .ofNat x) (.int $ .ofNat y) =>
    return .zeros $ .append (.lift $ .const x) $ .append (.lift $ .const y) $ .nil
  | .tensor _ _ => throw "Complex expressions as arguments of tensor"
  | .matMult a b => .matmul <$> toPyExpr a <*> toPyExpr b
  | .neg a => .neg <$> toPyExpr a
  | .var n => return .var n
  | .int x => return .int x

end Adapter

#check Parser
open Parser Char ASCII

def Py.Shape.ofList (l : List Nat) : Py.Shape :=
  match l with
  | [] => .nil
  | x::xs => .append (.lift $ .const x) $ ofList xs

partial def typeParser : SimpleParser Substring Char Py.Typ := takeMany whitespace *> (
  string "int" *> pure .int <|>
  string "float" *> pure .float <|>
  string "Tensor" *> takeMany whitespace *> char '(' *> .tensor <$> .ofList <$> tensorDims
) where tensorDims : SimpleParser Substring Char $ List Nat := takeMany whitespace *> (
  .pure <$> parseNat <* takeMany whitespace <* char ')' <|>
  .cons <$> (parseNat <* takeMany whitespace <* char ',') <*> tensorDims
)

def parseType (s : String) : Except String Py.Typ :=
  match typeParser.run s.toSubstring with
  | .ok _ e => .ok e
  | .error _ _ => .error s!"Unable to parse type; given input {s}"

def Py.Shape.toString (s : Py.Shape) := match s with
  | .nil => "∅"
  | .append a b => s!"({toString a}) ++ ({toString b})"
  | .var n => s!"var {n}"
  | .lift $ .var n => s!"[{n}]"
  | .lift $ .const k => s!"[{k}]"

instance : ToString Py.Shape where toString x := x.toString

def Py.Typ.toString (x : Py.Typ) := match x with
  | .tensor s => s!"TENSOR {s}"
  | .list l => s!"LIST {toString l}"
  | .float => "FLOAT"
  | .int => "INT"

instance : ToString Py.Typ where toString x := x.toString

#eval parseType "  Tensor  (  8  ,  5  )  "

open Lean Elab Command PrettyPrinter

def doElab (outFile : System.FilePath) : CommandElabM Unit := do
  match process_function () with
  | .none => throwError "Failed to process function"
  | .some ⟨name, body, input, output⟩ =>
    let ⟨body, ret⟩ := body.splitAt $ body.length - 1
    let fdi : Py.FunDefIn <- (⟨.mkSimple name, ·, ·, ·, ·⟩)
      <$> params input
      <*> statements body
      <*> retExpr ret
      <*> retType output
    let formatted <- liftCoreM $ ppCommand $ <- ElabPy.func fdi.toFunDef
    IO.FS.writeFile outFile $ "import ShapeChecker\n" ++ formatted.pretty
where
  retExpr (l : List Adapter.Stmt) : CommandElabM Py.Expr :=
    match l.get? 0 with
    | .none => throwError "Empty function body"
    | .some $ .assign _ _ => throwError "Assignment statement in return position"
    | .some $ .ret e => match e.toPyExpr with
      | .error _ => throwError "Invalid expression in return position"
      | .ok retval => return retval
  statements (l : List Adapter.Stmt) : CommandElabM $ List Py.StmtIn := do
    let mut body := []
    for x in l do match x with
      | .ret _ => throwError "Function not in return-normal form"
      | .assign n v => match v.toPyExpr with
        | .error _ => throwError "Invalid expression in function body"
        | .ok e => body := .assign n e :: body
    return body
  retType (s : String) : CommandElabM $ Py.Typ := do
    match parseType s with
    | .error _ => throwError s!"Invalid type specification in output; output was {s}"
    | .ok rettyp => return rettyp
  params (i : List $ String × String) : CommandElabM $ List $ Name × Py.Typ := do
    let mut params := []
    for ⟨n, t⟩ in i do match parseType t with
      | .error _ => throwError "Invalid type specification in parameters"
      | .ok o => params := ⟨.mkSimple n, o⟩ :: params
    return params

instance : EmptyCollection Context := ⟨{
  fileName     := ""
  fileMap      := ⟨"", #[]⟩
  tacticCache? := .none
  snap?        := .none
  cancelTk?    := .none
}⟩

def main (s : List String) : IO Unit := do
  match s.get? 0 with
  | .none => throw $ .userError "Did not pass output file name."
  | .some outfile =>
    -- Evidently, this doesn't generalize; look for the Lean installation in the system environment variable
    initSearchPath ⟨"/Users/markpock/.elan/toolchains/leanprover--lean4---v4.13.0"⟩
    let result <- doElab ⟨outfile⟩
      |>.run ∅
      |>.run {env := <- importModules #[`Init] .empty, maxRecDepth := 1000}
      |>.toIO'
    match result with
    | .ok _ => .println "Compilation complete"
    | .error $ .error ref msg => throw $ .userError s!"Lean user exception {<- msg.toString} at syntax {ref}"
    | .error $ .internal _ _  => throw $ .userError "Unidentifiable Lean internal error"


-- #check Lean.Options
-- #check EIO.toIO
-- #check Lean.Exception
-- #check Lean.Elab.Command.Context
-- #check Lean.Elab.Command.State
-- #check Lean.Environment
-- #check StateRefT'
