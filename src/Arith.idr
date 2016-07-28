module Arith

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

data Expr = Num Integer
          | Add Expr Expr
          | Sub Expr Expr
          | Mul Expr Expr
          | Div Expr Expr

instance Show Expr where
  show (Num i) = show i
  show (Add e1 e2) = "(" ++ show e1 ++ " + " ++ show e2 ++ ")" 
  show (Sub e1 e2) = "(" ++ show e1 ++ " - " ++ show e2 ++ ")"
  show (Mul e1 e2) = "(" ++ show e1 ++ " * " ++ show e2 ++ ")"
  show (Div e1 e2) = "(" ++ show e1 ++ " / " ++ show e2 ++ ")"

eval : Expr -> Integer
eval (Num i) = i
eval (Add e1 e2) = eval e1 + eval e2
eval (Sub e1 e2) = eval e1 - eval e2
eval (Mul e1 e2) = eval e1 * eval e2
eval (Div e1 e2) = eval e1 `div` eval e2

fromDigits : List (Fin 10) -> Integer
fromDigits = foldl (\a, b => 10 * a + cast b) 0

trim : Char -> Parser Char
trim c = spaces *> char c <* spaces

mutual
  parseAtom : Parser Expr
  parseAtom = (do digits <- some digit
                  return $ Num $ fromDigits digits)
          <|> (do char '-'
                  digits <- some digit
                  return $ Num (- (fromDigits digits)))
          <|> (do char '('
                  e <- parseExpr
                  char ')'
                  return e)

  parseFactor : Parser Expr
  parseFactor = (do f <- parseAtom
                    trim '*'
                    a <- parseAtom
                    return $ Mul f a)
            <|> (do f <- parseAtom
                    trim '/'
                    a <- parseAtom
                    return $ Div f a)
            <|> parseAtom

  parseTerm : Parser Expr
  parseTerm = (do t <- parseFactor
                  trim '+'
                  f <- parseFactor
                  return $ Add t f)
          <|> (do t <- parseFactor
                  trim '-'
                  f <- parseFactor
                  return $ Sub t f)
          <|> parseFactor

  parseExpr : Parser Expr
  parseExpr = parseTerm

toplevel : String -> String
toplevel input = case parse parseExpr input of
                   Right res => show $ eval res
                   Left err  => err
