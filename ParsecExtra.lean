import Lean.Data.Parsec
import Lean.Data.Position

namespace Lean.Parsec
open Parsec.ParseResult

/-- `getPos` returns `Pos` of current state -/
def getPos : Parsec String.Pos := fun it : String.Iterator =>
  success it it.pos

/-- `getPosition` returns `Position` of current state -/
def getPosition : Parsec Position := fun it : String.Iterator =>
  success it <| it.s.toFileMap.toPosition it.pos

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

private
def between (before : Parsec Unit) (p : Parsec a) (after : Parsec Unit) := do
  before; ws; let r ← p; ws; after; ws
  return r
/-- `parens` wrap expression with parenthesis `(` and `)` -/
def parens (p : Parsec a) : Parsec a := between (skipChar '(') p (skipChar ')')
/-- `braces` wrap expression with braces `{` and `}` -/
def braces (p : Parsec a) : Parsec a := between (skipChar '{') p (skipChar '}')


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
  |> «mixfix» [skipChar '*' *> return (· * ·), skipChar '/' *> return (· / ·)]
  |> «mixfix» [skipChar '+' *> return (· + ·), skipChar '-' *> return (· - ·)]

#eval parseExpr.run "1"
#eval parseExpr.run "1+2*3"
#eval parseExpr.run "-1+2*3"
```
-/

/-- `«mixfix»` is a part of expression combinators -/
def «mixfix» (opList : List (Parsec (α → α → α))) (tm : Parsec α)
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

/-- `«prefix»` is a part of expression combinators, stands for operator like negative, e.g. `-3` -/
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

/-- `«postfix»` is a part of expression combinators, stands for operator like factorial, e.g. `3!` -/
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

section test

#eval (parens (skipChar 'a')).run "(a)"
#eval (parens (skipChar 'a')).run "( a  )"
example : (tryP <| skipChar 'a').run "" = .ok .none := rfl
example : (tryP <| skipChar 'a').run "a" = .ok (.some ()) := rfl

private def parseInt : Parsec Nat := do
  return (← many1Chars <| satisfy (λ c => c.isDigit)).toNat!

private partial def factorial : Int → Int
  | 0 => 1
  | n =>
    if n < 0 then panic! "factorial: negative input"
    else n * factorial (n-1)

private def parseExpr : Parsec Int :=
  parseInt
  |> «postfix» [skipChar '!' *> return factorial]
  |> «prefix» [skipChar '-' *> return (- ·)]
  |> «mixfix» [skipChar '*' *> return (· * ·), skipChar '/' *> return (· / ·)]
  |> «mixfix» [skipChar '+' *> return (· + ·), skipChar '-' *> return (· - ·)]

#eval parseExpr.run "1"
#eval parseExpr.run "1+2*3"
#eval parseExpr.run "-1+2*3"
#eval parseExpr.run "6!/2"

end test

end Lean.Parsec
