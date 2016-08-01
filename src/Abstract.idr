module Abstract

import Operator
import Core

%access public export

TEnv : Type
TEnv = List (String, T)

checkUnary : UnaryOp -> T -> Either String T
checkUnary Not BoolT = Right BoolT
checkUnary Neg IntT = Right IntT
checkUnary op t = Left $ "Mismatched argument for " ++ show op ++ " " ++ show t

checkBinary : BinaryOp -> T -> T -> Either String T
checkBinary Add IntT IntT = return IntT
checkBinary Sub IntT IntT = return IntT
checkBinary Mul IntT IntT = return IntT
checkBinary Div IntT IntT = return IntT
checkBinary And BoolT BoolT = return BoolT
checkBinary Or BoolT BoolT = return BoolT
checkBinary LT IntT IntT = return BoolT
checkBinary LE IntT IntT = return BoolT
checkBinary GE IntT IntT = return BoolT
checkBinary GT IntT IntT = return BoolT
checkBinary EQ t1 t2 with (t1 == t2) | True = return BoolT
checkBinary op t1 t2 = Left $ "Mismatched binary types for " ++ show t1 ++ " " ++ show op ++ " " ++ show t2

typeCheck : Expr -> TEnv -> Either String T
typeCheck (Literal (IntV _)) _ = return IntT
typeCheck (Literal (BoolV _)) _ = return BoolT
typeCheck (Unary op e) env = do t <- typeCheck e env
                                checkUnary op t
typeCheck (Binary op e1 e2) env = do t1 <- typeCheck e1 env
                                     t2 <- typeCheck e2 env
                                     checkBinary op t1 t2
typeCheck (If p c a) env = do pt <- typeCheck p env
                              if BoolT /= pt
                              then Left $ "Conditional must return a boolean: " ++ show p
                              else do ct <- typeCheck c env
                                      at <- typeCheck a env
                                      if ct /= at
                                      then Left $ "Result types are not the same in " ++ show c ++ ", " ++ show a
                                      else typeCheck c env
typeCheck (Variable x) env = case lookup x env of
                               Just t => return t
                               Nothing => Left $ "Type of variable " ++ x ++ " not found"
typeCheck (Declare x exp body) env =  typeCheck body env'
  where env' = case typeCheck exp env of
                 Right t => (x, t) :: env'
                 Left _ => []
typeCheck (Function x t body) env = do bt <- typeCheck body env'
                                       return $ FunT t bt
  where env' = (x, t) :: env
typeCheck (Call fun arg) env = do at <- typeCheck arg env
                                  ft <- typeCheck fun env
                                  case ft of
                                    FunT a b => if a /= at
                                                then Left "Invalid argument type"
                                                else return b
                                    _ => Left "Expected function"

check : Expr -> T
check exp = case typeCheck exp [] of
              Right t => t
              Left _ => NilT
