module Main

import System

import Core
import Parser
import Eval
import Abstract
import Lightyear.Strings

processArgs : List String -> Maybe String
processArgs [_, x] = Just x
processArgs _ = Nothing

processFile : String -> IO ()
processFile fname = do
  file <- readFile fname
  case file of
    Right contents => case parse parseExpr contents of
                        Right res => case check res of
                                       ErrT err => putStrLn err
                                       _ => putStrLn $ show $ exec res
                        Left err  => putStrLn err
    Left err => putStrLn $ show err

main : IO ()
main = do
  args <- getArgs
  case processArgs args of
    Just fname => processFile fname
    Nothing => putStrLn "anatomy"
