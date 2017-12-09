instance Show Expr where
  show (V vector) = show vector
  show (VO vectorOp expr1 expr2) = "{" ++ show vectorOp ++ ", " ++ show expr1 ++ ", " ++ show expr2 ++ "}"
  show (SO scalarOp intExpr expr) = "{" ++ show scalarOp ++ ", " ++ show intExpr ++ ", " ++ show expr ++ "}"

instance Show IntExpr where
  show (I n) = show n
  show (NO normOp expr) = "{" ++ show normOp ++ ", " ++ show expr ++ "}"

instance Show VectorOp where
  show Add = "'add'"
  show Sub = "'sub'"
  show Dot = "'dot'"

instance Show ScalarOp where
  show Mul = "'mul'"
  show Div = "'div'"

instance Show NormOp where
  show NormOne = "'norm_one'"
  show NormInf = "'norm_inf'"
