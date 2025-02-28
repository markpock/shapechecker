
def coerceTermIdent (t : TSyntax `term) : TSyntax `ident :=
  ⟨t.raw⟩

#check Lean.Parser.Term.bracketedBinder
def examplific : Name -> CommandElabM (TSyntax `command) := λ n => do
  dbg_trace s!"hello"
  let ex := mkNode `Lean.Parser.Term.bracketedBinder #[
    mkAtom "(",
    mkIdent `x,
    mkAtom ":",
    mkIdent `Nat,
    mkAtom ")"
  ]
  let lst := #[mkIdent `x, mkIdent `y]

  `(def $(mkIdent n) : Nat -> Nat -> Nat := λ $(lst)* => $(mkIdent `x) + 3)



  -- q


  -- let x : TSyntax `Lean.Parser.Term.bracketedBinder <- quote (`u : `Unit)
  -- -- let x : TSyntax `Lean.Parser.Term.bracketedBinder <- `(($(mkIdent `u) : $(mkIdent `Unit)))
  -- `(def $(mkIdent n) ($(mkIdent `x) : $(mkIdent `Nat)) : $(mkIdent `Nat) := 3)





-- syntax (name := tm) "tensorMul!" term:max term:max: term


-- def tensorMul (al bl : List Nat) : TermElabM Expr := do
--   -- Example of compile-time validation
--   if al.length ≠ bl.length then
--     throwError "Tensor multiplication requires matching dimensions."
--   else
--     -- if al.length == 1 then
--     --   let aLast := al.getLast!
--     --   let bLast := bl.getLast!
--     --   pure (elabTerm `(Tensor [1, 2, 3]))
--     -- Generate whatever Py.Expr is needed
--     pure (mkConst ``Unit.unit)

-- #check `(1 + 2 + 3)
-- #check Expr

-- @[term_elab tm]
-- unsafe def elabTensorMul : TermElab := fun stx expectedType? => do
--   match stx with
--   | `(tensorMul! $a $b) => do
--       -- Elaborate `a` and `b` to get their types
--       let aExpr <- elabTerm a none
--       let bExpr <- elabTerm b none
--       let aType <- inferType aExpr
--       let bType <- inferType bExpr

--       -- Extract the list indices from `Tensor al`
--       let getListFromTensor (t : Expr) : TermElabM (Option Expr) := do
--         match t with
--         | Expr.app (Expr.const ``Tensor _) x => pure (some x)  -- Assuming it's just one number for now
--         | _ => pure none

--       let al? ← getListFromTensor aType
--       let bl? ← getListFromTensor bType

--       match al?, bl? with
--       | some al, some bl =>
--         logInfo m!"Here we are {al} {bl}"
--         let listNatType ← mkAppM ``List #[mkConst ``Nat]
--         let al' : List Nat <- evalExpr (List Nat) listNatType al
--         let bl' : List Nat <- evalExpr (List Nat) listNatType bl
--         logInfo m!"Here we are due {al'}, {bl'}"
--         tensorMul al' bl'
--       | ra, rb =>
--         logError "Could not evaluate types of tensors at compile-time."
--         pure (mkConst ``Unit.unit)
--   | _ => throwError "Invalid syntax for tensorMul!"



-- import Lean

-- open Lean Meta Elab Term

-- inductive Expre
--   | var (name : String)
--   | lit (n : Nat)
--   | app (fn : Expre) (arg : Expre)
--   | lam (param : String) (body : Expre)

-- inductive Stmt
--   | ret (expr : Expre)
--   | letBind (name : String) (value : Expre) (body : Stmt)

-- inductive Func
--   | func (name : String) (params : List (String × Expr)) (body : Stmt)


-- def exprToLean (e : Expre) : MetaM Expr := do
--   match e with
--   | Expre.var name     => pure (mkFVar (← mkFreshFVarId))  -- Variable reference
--   | Expre.lit n        => pure (mkNatLit n)  -- Literal integer
--   | Expre.app f x      => do
--       let fLean ← exprToLean f
--       let xLean ← exprToLean x
--       pure (mkApp fLean xLean)  -- Function application
--   | Expre.lam param b  => do
--       -- let x ← mkFreshFVarId
--       let body ← exprToLean b
--       pure (mkLambda (Name.str .anonymous param) BinderInfo.default (← inferType body) body)  -- Lambda abstraction

-- def stmtToLean (s : Stmt) : MetaM Expr := do
--   match s with
--   | Stmt.ret e         => exprToLean e
--   | Stmt.letBind n v b =>
--       let val ← exprToLean v
--       let body ← stmtToLean b
--       let hm ← mkFreshFVarId
--       let res <- mkLetFVars #[] val body
--       res  -- Let-binding

-- def funcToLean (f : Func) : MetaM Expr :=
--   match f with
--   | Func.func name params body => do
--       let paramFVars ← params.mapM (fun (n, ty) => mkFreshFVarId)
--       let paramTypes ← params.mapM (fun (_, ty) => pure ty)
--       let bodyExpr ← stmtToLean body
--       pure (mkLambdaFVars paramFVars bodyExpr)


import Lean

open Lean Elab Command Term Meta

syntax "myfun" ident ident "ht" ident "dec" term : command

#check CommandElabM

elab_rules : command
| `(myfun $fn:ident $arg:ident ht $ty:ident dec $body:term) => do
    let fnName := mkIdent fn.getId
    let argName := mkIdent arg.getId
    let funDecl <- `(def $fnName ($argName : $ty) := $body)
    let f <- liftCoreM $ PrettyPrinter.ppCommand funDecl
    dbg_trace s!"{f}"
    elabCommand funDecl

myfun a b ht Nat dec b + b

#check a 3

#eval a 3
