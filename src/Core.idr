module Core

import Operator

%access public export

mutual
  Env : Type
  Env = List (String, Value)

  data Value = Nil
             | IntV Integer
             | BoolV Bool
             | ClosureV String Expr Env

  implementation Eq Value where
    Nil == Nil = True
    (IntV a) == (IntV b) = a == b
    (BoolV a) == (BoolV b) = a == b
    --(ClosureV n1 e1 env1) == (ClosureV n2 e2 env2) = n1 == n2 && e1 == e2 && env1 == env2
    _ == _ = False

  implementation Show Value where
    show Nil = "nil"
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
            | Function String Expr
            | Call Expr Expr

  implementation Eq Expr where
    (Literal v1) == (Literal v2) = v1 == v2
    (Unary o1 e1) == (Unary o2 e2) = o1 == o2 && e1 == e2
    (Binary o1 e11 e12) == (Binary o2 e21 e22) = o1 == o2 && e11 == e21 && e12 == e22
    (If p1 c1 a1) == (If p2 c2 a2) = p1 == p2 && c1 == c2 && a1 == a2
    (Variable x1) == (Variable x2) = x1 == x2
    (Declare n1 v1 b1) == (Declare n2 v2 b2) = n1 == n2 && v1 == v2 && b1 == b2
    (Function n1 b1) == (Function n2 b2) = n1 == n2 && b1 == b2
    (Call e11 e12) == (Call e21 e22) = e11 == e21 && e12 == e22
    _ == _ = False

  implementation Show Expr where
    show (Literal i) = show i
    show (Unary op e) = show op ++ " " ++ show e
    show (Binary op e1 e2) = "(" ++ show e1 ++ " " ++ show op ++ " " ++ show e2 ++ ")"
    show (If pred cons alt) = "(if " ++ show pred ++ " then " ++ show cons ++ " else " ++ show alt ++ ")"
    show (Variable x) = x
    show (Declare name val body) = "let " ++ name ++ " = " ++ show val ++ " in " ++ show body ++ ")"

unary : UnaryOp -> Value -> Value
unary Not (BoolV b) = BoolV (not b)
unary Neg (IntV i) = IntV (-i)

binary : BinaryOp -> Value -> Value -> Value;
binary Add (IntV a) (IntV b) = IntV $ a + b
binary Sub (IntV a) (IntV b) = IntV $ a - b
binary Mul (IntV a) (IntV b) = IntV $ a * b
binary Div (IntV a) (IntV b) = IntV $ a `div` b
binary And (BoolV a) (BoolV b) = BoolV $ a && b
binary Or (BoolV a) (BoolV b) = BoolV $ a || b
binary LT (IntV a) (IntV b) = BoolV $ a < b
binary LE (IntV a) (IntV b) = BoolV $ a <= b
binary GT (IntV a) (IntV b) = BoolV $ a > b
binary GE (IntV a) (IntV b) = BoolV $ a >= b
binary EQ a b = BoolV $ a == b

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
eval (Function name body) env = return $ ClosureV name body env
eval (Call fn arg) env = do (ClosureV x body cenv) <- eval fn env
                            argv <- eval arg env
                            let env' = (x, argv) :: cenv
                            eval body env'

exec : Expr -> Value
exec exp = case eval exp [] of
             Just result => result
             Nothing => Nil
