module Operators

%access public export

data BinaryOp = Add
              | Sub
              | Mul
              | Div
              | And
              | Or
              | GT
              | LT
              | GE
              | LE
              | EQ

implementation Show BinaryOp where
  show Add = "+"
  show Sub = "-"
  show Mul = "*"
  show Div = "/"
  show And = "&&"
  show Or = "||"
  show GT = ">"
  show LT = "<"
  show GE = ">="
  show LE = "<="
  show EQ = "=="

implementation Eq BinaryOp where
  Add == Add = True
  Sub == Sub = True
  Mul == Mul = True
  Div == Div = True
  And == And = True
  Or == Or = True
  GT == GT = True
  LT == LT = True
  GE == GE = True
  LE == LE = True
  EQ == EQ = True
  _ == _ = False

data UnaryOp = Neg
             | Not

implementation Show UnaryOp where
  show Neg = "-"
  show Not = "!"

implementation Eq UnaryOp where
  Neg == Neg = True
  Not == Not = True
  _ == _ = False

