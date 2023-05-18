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

end Lean.Parsec
