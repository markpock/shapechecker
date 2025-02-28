import Backend.PythonAdapter

@[match_pattern] def Tree.singleton (data : Int) : Tree := .branch data .nil .nil

def Tree.toString : Tree -> String
  | .nil => "·"
  | .singleton data => s!"{data}"
  | .branch data left right =>
    let x := left.toString
    let y := toString right
    s!"{data}, \{{x}} \{{y}}"

instance : ToString (Tree) := ⟨Tree.toString⟩


def main : IO Unit :=
  let x := ct ()
  match x with
  | .none => IO.println "Geh"
  | .some t => IO.println s!"Hello¬{t}"


-- #eval! (ct (1 : UInt32))
