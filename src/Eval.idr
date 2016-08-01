module Eval

import Operator
import Core

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
eval (Declare name val body) env = eval body env'
  where env' = case eval val env of
                 Just v => (name, v) :: env' -- for recursion
                 Nothing => []
eval (Function name _ body) env = return $ ClosureV name body env
eval (Call fn arg) env = do (ClosureV x body cenv) <- eval fn env
                            argv <- eval arg env
                            let env' = (x, argv) :: cenv
                            eval body env'

exec : Expr -> Value
exec exp = case eval exp [] of
             Just result => result
             Nothing => Nil


