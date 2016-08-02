module Test

import Core
import Parser
import Eval
import Abstract

%access public export

assertEq : Eq a => (given : a) -> (expected : a) -> IO Unit
assertEq g e = if g == e
               then putStrLn "Test Passed"
               else putStrLn "Test Failed"

spec : IO Unit
spec = do
  let ast = toplevel "var n = function(n : int) { if(n <= 1) 1 else n + 1 }; n(5)"
  case ast of
    Just expr => do assertEq (show expr) "let n = fn(n : int){(if (n <= 1) then 1 else (n + 1))} in n(5)"
                    case check expr of
                      ErrT err => putStrLn err
                      t => assertEq t IntT
                    case exec expr of
                      NilV => putStrLn "eval failed"
                      v => assertEq v (IntV 6)
    Nothing => putStrLn "parse failed"
