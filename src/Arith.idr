module Arith

import public Lightyear
import public Lightyear.Char
import public Lightyear.Strings

import Value
import Operator

data Expr = Literal Value
          | Unary UnaryOp Expr
          | Binary BinaryOp Expr Expr
          | If Expr Expr Expr
          | Variable String
          | Declare String Expr Expr

implementation Show Expr where
  show (Literal i) = show i
  show (Unary op e) = show op ++ " " ++ show e
  show (Binary op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
  show (If pred cons alt) = "(if " ++ show pred ++ " then " ++ show cons ++ " else " ++ show alt ++ ")"
  show (Variable x) = x
  show (Declare name val body) = "let " ++ name ++ " = " ++ show val ++ " in " ++ show body ++ ")"

eval : Expr -> Env -> Maybe Value
eval (Literal v) _ = return v
eval (Unary op e) env = do v <- eval e env
                           return $ unary op v
eval (Binary op e1 e2) env = do v1 <- eval e1 env
                                v2 <- eval e2 env
                                return $ binary op v1 v2
eval (If pred cons alt) env = do (BoolV p) <- eval pred env
                                 if p then eval cons env
                                      else eval alt env
eval (Variable x) env = lookup x env
eval (Declare name val body) env = do v <- eval val env
                                      let env' = (name, v) :: env
                                      eval body env'

fromDigits : List (Fin 10) -> Integer
fromDigits = foldl (\a, b => 10 * a + cast b) 0

trim : Char -> Parser Char
trim c = spaces *> char c <* spaces

trims : String -> Parser String
trims s = spaces *> string s <* spaces

identifier : Parser String
identifier = spaces *> map pack (some (satisfy isAlpha)) <* spaces

mutual
  parseAtom : Parser Expr
  parseAtom = (do digits <- some digit
                  return $ Literal $ IntV $ fromDigits digits)
          <|> (do trims "true"
                  return $ Literal $ BoolV True)
          <|> (do trims "false"
                  return $ Literal $ BoolV False)
          <|> (do char '-'
                  a <- parseAtom
                  return $ Unary Neg a)
          <|> (do char '!'
                  a <- parseAtom
                  return $ Unary Not a)
          <|> (do id <- identifier
                  return $ Variable id)
          <|> (do char '('
                  e <- parseExpr
                  char ')'
                  return e)

  parseFactor : Parser Expr
  parseFactor = (do a <- parseAtom
                    trim '*'
                    f <- parseFactor
                    return $ Binary Mul a f)
            <|> (do a <- parseAtom
                    trim '/'
                    f <- parseFactor
                    return $ Binary Div a f)
            <|> parseAtom

  parseTerm : Parser Expr
  parseTerm = (do f <- parseFactor
                  trim '+'
                  t <- parseTerm
                  return $ Binary Add f t)
          <|> (do f <- parseFactor
                  trim '-'
                  t <- parseTerm
                  return $ Binary Sub f t)
          <|> parseFactor

  parseComp : Parser Expr
  parseComp = (do t <- parseTerm
                  trims "=="
                  c <- parseComp
                  return $ Binary EQ t c)
          <|> (do t <- parseTerm
                  trim '<'
                  c <- parseComp
                  return $ Binary LT t c)
          <|> (do t <- parseTerm
                  trim '>'
                  c <- parseComp
                  return $ Binary GT t c)
          <|> (do t <- parseTerm
                  trims "<="
                  c <- parseComp
                  return $ Binary LE t c)
          <|> (do t <- parseTerm
                  trims ">="
                  c <- parseComp
                  return $ Binary GE t c)
          <|> parseTerm

  parseAnd : Parser Expr
  parseAnd = (do c <- parseComp
                 trims "&&"
                 a <- parseAnd
                 return $ Binary And c a)
         <|> parseComp

  parseOr : Parser Expr
  parseOr = (do a <- parseAnd
                trims "||"
                o <- parseOr
                return $ Binary Or a o)
        <|> parseAnd

  parseExpr : Parser Expr
  parseExpr = (do trims "var"
                  id <- identifier
                  trim '='
                  val <- parseExpr
                  trim ';'
                  body <- parseExpr
                  return $ Declare id val body)
          <|> (do trims "if"
                  trim '('
                  pred <- parseExpr
                  trim ')'
                  cons <- parseExpr
                  trims "else"
                  alt <- parseExpr
                  return $ If pred cons alt)
          <|> parseOr

toplevel : String -> String
toplevel input = case parse parseExpr input of
                   Right res => case eval res [] of
                                  Just r => show r
                                  Nothing => "nothing"
                   Left err  => err
