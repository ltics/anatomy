module Core

import Value
import Operator

%access public export

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

