module Value

import Operator

%access public export

data Value = IntV Integer
           | BoolV Bool

Env : Type
Env = List (String, Value)

implementation Eq Value where
  (IntV a) == (IntV b) = a == b
  (BoolV a) == (BoolV b) = a == b
  _ == _ = False

implementation Show Value where
  show (IntV n) = show n
  show (BoolV True) = "true"
  show (BoolV False) = "false"

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
