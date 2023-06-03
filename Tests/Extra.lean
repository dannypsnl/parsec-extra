import ParsecExtra
import LSpec

open LSpec
open Lean Lean.Parsec

private instance [BEq α] : BEq (Except ε α) where
  beq a b := match a, b with
    | Except.ok a, Except.ok b => a == b
    | _, _ => false

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
  |> «infixR» [skipString "^" *> return (Int.pow · ·.toNat)]
  |> «infixL» [skipChar '*' *> return (· * ·), skipChar '/' *> return (· / ·)]
  |> «infixL» [skipChar '+' *> return (· + ·), skipChar '-' *> return (· - ·)]

def main := lspecIO $
  test "parens test"
    ((parens (skipChar 'a')).run "(a)" |> Except.isOk)
  $ test "parens with some space"
    ((parens (skipChar 'a')).run "( a  )" |> Except.isOk)
  $ test "tryP fail should return Option.none"
    ((tryP <| skipChar 'a').run "" |> (· == .ok .none))
  $ test "tryP success should return Option.some and wrap parser result"
    ((tryP <| skipChar 'a').run "a" |> (· == .ok (.some ())))
  $ group "parse expression" $
    test "integer"
      (parseExpr.run "1" == .ok 1)
    $ test "multiple first"
      (parseExpr.run "1+2*3" == .ok 7)
    $ test "prefix operator"
      (parseExpr.run "-1+2*3" == .ok 5)
    $ test "postfix operator"
      (parseExpr.run "6!/2" == .ok 360)
    $ test "right associative operator"
      (parseExpr.run "-1+2^2^3" == .ok 255)
