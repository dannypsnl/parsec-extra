import Lean.Data.Parsec
import Lean.Data.Position

namespace Lean.Parsec
open Parsec.ParseResult

/-- `getPos` returns `String.Pos` of current state -/
def getPos : Parsec String.Pos := fun it : String.Iterator =>
  success it it.pos

/-- `getPosition` returns `Position` of current state -/
def getPosition : Parsec Position := fun it : String.Iterator =>
  success it <| it.s.toFileMap.toPosition it.pos

/-- `withPosition` runs a parser, and wraps the result with start and end position -/
def withPosition {α} (p : Parsec α) (f : Position → Position → α → α) : Parsec α := do
  let startPos ← getPosition
  let result ← p
  let endPos ← getPosition
  return f startPos endPos result

/-- `runFilename` will report pretty source location with `filepath`, line, column pair -/
def runFilename (p : Parsec α) (filepath : System.FilePath) (s : String) : Except String α :=
  match p s.mkIterator with
  | .success _ res => Except.ok res
  | .error it err  =>
    let position := it.s.toFileMap.toPosition it.pos
    Except.error s!"{filepath}:{position.line}:{position.column}: {err}"

/-- `tryP` run a parser, if fail then rollback and return `Option.none` -/
def tryP (p : Parsec a) : Parsec (Option a) := λ it =>
  match p it with
  | .success rem a => .success rem a
  | .error _ _ => .success it .none

private def between (before : Parsec Unit) (p : Parsec a) (after : Parsec Unit)
  (user_ws : Parsec Unit := ws) := do
  before; user_ws; let r ← p; user_ws; after; user_ws
  return r
/-- `parens` wrap expression with parenthesis `(` and `)` -/
def parens (p : Parsec a) (user_ws : Parsec Unit := ws) : Parsec a :=
  between (skipChar '(') p (skipChar ')') user_ws
/-- `brackets` wrap expression with parenthesis `[` and `]` -/
def brackets (p : Parsec a) (user_ws : Parsec Unit := ws) : Parsec a :=
  between (skipChar '[') p (skipChar ']') user_ws
/-- `braces` wrap expression with braces `{` and `}` -/
def braces (p : Parsec a) (user_ws : Parsec Unit := ws) : Parsec a :=
  between (skipChar '{') p (skipChar '}') user_ws

/-!
## Expression Combinators

The correct way to expression combinators, is based on an atom parser, then stacking expression combinators via `|>` operator. The stack order is the precedence order.

Here is an example:

```lean
def parseInt : Parsec Int := do
  return (← many1Chars <| satisfy (λ c => c.isDigit)).toInt!

def parseExpr : Parsec Int :=
  parseInt
  |> «prefix» [skipChar '-' *> return (- ·)]
  |> «infixR» [skipString "^" *> return (Int.pow · ·.toNat)]
  |> «infixL» [skipChar '*' *> return (· * ·), skipChar '/' *> return (· / ·)]
  |> «infixL» [skipChar '+' *> return (· + ·), skipChar '-' *> return (· - ·)]

#eval parseExpr.run "1"
#eval parseExpr.run "1+2*3"
#eval parseExpr.run "-1+2*3"
```
-/

/-- `infixL` builds a infix left-associativity expression -/
def infixL (opList : List (Parsec (α → α → α))) (tm : Parsec α)
  : Parsec α := do
  let l ← tm
  let rs ← many do
    for findOp in opList do
      match ← tryP findOp with
      | .some f => return (f, ← tm)
      | .none => continue
    fail "cannot match any operator"
  return rs.foldl (fun lhs (bin, rhs) => (bin lhs rhs)) l

/-- `infixR` builds a infix right-associativity expression -/
partial def infixR (opList : List (Parsec (α → α → α))) (tm : Parsec α)
  : Parsec α := go #[]
  where
  go (ls : Array (α × (α → α → α))) : Parsec α := do
    let lhs ← tm
    for findOp in opList do
      match ← tryP findOp with
      | .some f => return ← go (ls.push (lhs, f))
      | .none => continue
    let rhs := lhs
    return ls.foldr (fun (lhs, bin) rhs => bin lhs rhs) rhs

/-- `«prefix»` builds a prefix expression, e.g. negative prefix `-3` -/
def «prefix» (opList : List $ Parsec (α → α)) (tm : Parsec α)
  : Parsec α := do
  let mut op := .none
  for findOp in opList do
    op ← tryP findOp
    if op.isSome then break
  match op with
  | .none => tm
  | .some f => return f (← tm)

/-- `«postfix»` builds a postfix expression, e.g. factorial `3!` -/
def «postfix» (opList : List $ Parsec (α → α)) (tm : Parsec α)
  : Parsec α := do
  let e ← tm
  let mut op := .none
  for findOp in opList do
    op ← tryP findOp
    if op.isSome then break
  match op with
  | .none => return e
  | .some f => return f e

end Lean.Parsec
