module Core

import Operator

%access public export

mutual
  data T = NilT
         | IntT
         | BoolT
         | FunT T T

  implementation Eq T where
    NilT == NilT = True
    IntT == IntT = True
    BoolT == BoolT = True
    (FunT t11 t12) == (FunT t21 t22) = t11 == t21 && t12 == t22
    _ == _ = False

  implementation Show T where
    show NilT = "nil"
    show IntT = "int"
    show BoolT = "bool"
    show (FunT t1 t2) = show t1 ++ " -> " ++ show t2

  Env : Type
  Env = List (String, Value)

  data Value = NilV
             | IntV Integer
             | BoolV Bool
             | ClosureV String Expr Env

  implementation Eq Value where
    NilV == NilV = True
    (IntV a) == (IntV b) = a == b
    (BoolV a) == (BoolV b) = a == b
    --(ClosureV n1 e1 env1) == (ClosureV n2 e2 env2) = n1 == n2 && e1 == e2 && env1 == env2
    _ == _ = False

  implementation Show Value where
    show NilV = "nil"
    show (IntV n) = show n
    show (BoolV True) = "true"
    show (BoolV False) = "false"
    show (ClosureV n e env) = "<fun>"

  data Expr = Literal Value
            | Unary UnaryOp Expr
            | Binary BinaryOp Expr Expr
            | If Expr Expr Expr
            | Variable String
            | Declare String Expr Expr
            | Function String T Expr
            | Call Expr Expr

  implementation Eq Expr where
    (Literal v1) == (Literal v2) = v1 == v2
    (Unary o1 e1) == (Unary o2 e2) = o1 == o2 && e1 == e2
    (Binary o1 e11 e12) == (Binary o2 e21 e22) = o1 == o2 && e11 == e21 && e12 == e22
    (If p1 c1 a1) == (If p2 c2 a2) = p1 == p2 && c1 == c2 && a1 == a2
    (Variable x1) == (Variable x2) = x1 == x2
    (Declare n1 v1 b1) == (Declare n2 v2 b2) = n1 == n2 && v1 == v2 && b1 == b2
    (Function n1 t1 b1) == (Function n2 t2 b2) = n1 == n2 && t1 == t2 && b1 == b2
    (Call e11 e12) == (Call e21 e22) = e11 == e21 && e12 == e22
    _ == _ = False

  implementation Show Expr where
    show (Literal i) = show i
    show (Unary op e) = show op ++ " " ++ show e
    show (Binary op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (If pred cons alt) = "(if " ++ show pred ++ " then " ++ show cons ++ " else " ++ show alt ++ ")"
    show (Variable x) = x
    show (Declare name val body) = "let " ++ name ++ " = " ++ show val ++ " in " ++ show body ++ ")"

