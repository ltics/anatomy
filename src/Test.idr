module Test

import Core
import Parser
import Eval
import Abstract

%access public export

spec : IO ()
spec = do
  let ast = toplevel "var n = function(n : int) { if(n <= 1) 1 else n + 1 }; n(5)"
  case ast of
    Just expr => do putStrLn $ show $ show expr == "let n = fn(n : int){(if (n <= 1) then 1 else (n + 1))} in n(5)"
                    case check expr of
                      ErrT err => putStrLn err
                      t => putStrLn $ show $ show t == "int"
                    case exec expr of
                      NilV => putStrLn "eval failed"
                      v => putStrLn $ show $ show v == "6"
    Nothing => putStrLn "parse failed"
