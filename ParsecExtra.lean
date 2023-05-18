import Lean.Data.Parsec
import Lean.Data.Position

namespace Lean.Parsec
open Parsec.ParseResult

def getPos : Parsec String.Pos := fun it : String.Iterator =>
  success it it.pos

def getPosition : Parsec Position := fun it : String.Iterator =>
  success it <| it.s.toFileMap.toPosition it.pos

def runFilename (p : Parsec α) (filepath : System.FilePath) (s : String) : Except String α :=
  match p s.mkIterator with
  | .success _ res => Except.ok res
  | .error it err  =>
    let position := it.s.toFileMap.toPosition it.pos
    Except.error s!"{filepath}:{position.line}:{position.column}: {err}"

def tryP (p : Parsec a) : Parsec (Option a) := λ it =>
  match p it with
  | .success rem a => .success rem a
  | .error _ _ => .success it .none

def between (before : Parsec Unit) (p : Parsec a) (after : Parsec Unit) := do
  before; let r ← p; after; ws
  return r
def parens (p : Parsec a) : Parsec a := between (skipChar '(') p (skipChar ')')
def braces (p : Parsec a) : Parsec a := between (skipChar '{') p (skipChar '}')

def binary (opList : List (Parsec (α → α → α))) (tm : Parsec α)
  : Parsec α := do
  let l ← tm
  let rs ← many opRhs
  return rs.foldl (fun lhs (bin, rhs) => (bin lhs rhs)) l
  where
    opRhs : Parsec ((α → α → α) × α) := do
      for findOp in opList do
        match ← tryP findOp with
        | .some f => return ⟨ f, ← tm ⟩
        | .none => continue
      fail "cannot match operator"

def «prefix» (opList : List $ Parsec (α → α)) (tm : Parsec α)
  : Parsec α := do
  let mut op := .none
  for findOp in opList do
    op ← tryP findOp
    if op.isSome then break
  let e ← tm
  match op with
  | .none => return e
  | .some f => return f e

section test

example : (parens (skipChar 'a')).run "(a)" = .ok () := rfl
example : (tryP <| skipChar 'a').run "" = .ok .none := rfl
example : (tryP <| skipChar 'a').run "a" = .ok (.some ()) := rfl

def parseInt : Parsec Int := do
  return (← many1Chars <| satisfy (λ c => c.isDigit)).toInt!

def parseExpr : Parsec Int :=
  parseInt
  |> «prefix» [skipChar '-' *> return (- ·)]
  |> binary [skipChar '*' *> return (· * ·), skipChar '/' *> return (· / ·)]
  |> binary [skipChar '+' *> return (· + ·), skipChar '-' *> return (· - ·)]

#eval parseExpr.run "1"
#eval parseExpr.run "1+2*3"
#eval parseExpr.run "-1+2*3"

end test

end Lean.Parsec
