module Expr where


type VarIndex = Int


data BiOp = Expr :+: Expr
          | Expr :*: Expr
          | Expr :-: Expr
          | Expr :/: Expr
    deriving (Eq,Ord )
data UnOp = Exp Expr
          | Log Expr
          | Sin Expr
          | Cos Expr
    deriving (Eq, Ord)

data Expr = Constant Double
          | Var VarIndex
          | Op BiOp
          | Fun UnOp
       deriving (Eq, Ord)
instance Show BiOp where
    show (e :+: e') = "(" ++ show e ++ "+" ++ show e' ++ ")"
    show (e :*: e') = "(" ++ show e ++ "*" ++ show e' ++ ")"
    show (e :-: e') = "(" ++ show e ++ "-" ++ show e' ++ ")"
    show (e :/: e') = "(" ++ show e ++ "/" ++ show e' ++ ")"
instance Show UnOp where
    show (Exp e) = "exp(" ++ show e ++ ")"
    show (Log e) = "log(" ++ show e ++ ")"
    show (Sin e) = "sin(" ++ show e ++ ")"
    show (Cos e) = "cos(" ++ show e ++ ")"


instance Show Expr where
    show (Constant x) = show x
    show (Var idx) = "x"++ show idx
    show (Op b) = show b
    show (Fun f) = show f

instance Num Expr where
    (Constant x) + (Constant y) = Constant $ x + y
    f + g = Op $ f :+: g
    (Constant x) * (Constant y) = Constant $ x * y
    f * g = Op $ f :*: g
    (Constant x) - (Constant y) = Constant $ x - y
    f - g = Op $ f :-: g

    fromInteger x = Constant $ fromInteger x
    abs _ = error "abs is not differentiable"
    signum _ = error "signum is not differentiable"

instance Fractional Expr where
    (Constant x) / (Constant y) = Constant $ x / y
    e / e' = Op $ e :/: e'
    fromRational e = Constant $ fromRational e

instance Floating Expr where
    pi = Constant pi
    exp = Fun . Exp 
    log = Fun . Log
    sin = Fun . Sin
    cos = Fun . Cos



type Adjoint = Double

evalFun :: UnOp -> Double -> Double
evalFun (Exp _) x = exp x
evalFun (Log _) x = log x
evalFun (Sin _) x = sin x
evalFun (Cos _) x = cos x

evalOp :: BiOp -> Double -> Double -> Double
evalOp (_ :+: _) x y = x+y
evalOp (_ :-: _) x y = x-y
evalOp (_ :*: _) x y = x*y
evalOp (_ :/: _) x y = x/y

diffFun :: UnOp -> Double -> Adjoint -> Adjoint
diffFun (Exp x) xval adj = adj*exp xval 
diffFun (Log x) xval adj = adj*1/xval
diffFun (Sin x) xval adj = adj*cos xval
diffFun (Cos x) xval adj = -adj*sin xval

-- Quantités à ajouter à l'adjoint des deux expressions de l'operation
diffOp :: BiOp ->  (Double -> Double -> Adjoint -> (Adjoint, Adjoint))
diffOp (x :+: y) xval yval adj = (adj,adj)
diffOp (x :-: y) xval yval adj = (adj,negate adj)
diffOp (x :*: y) xval yval adj = (yval*adj, xval*adj)
diffOp (x :/: y) xval yval adj = (yval/yval^2 * adj, -xval/yval^2*adj)

operands :: BiOp -> (Expr,Expr)
operands (x :+: y) = (x,y)
operands (x :-: y) = (x,y)
operands (x :*: y) = (x,y)
operands (x :/: y) = (x,y)

variable :: UnOp -> Expr
variable (Exp x) = x
variable (Log x) = x
variable (Sin x) = x
variable (Cos x) = x
