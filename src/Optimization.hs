module Optimization where


import Expr
import BackProp
import qualified Data.Map as M
import qualified Data.List as L


data Params = V Values
            | S Double


data Constraint = Expr :<: Expr
                | Expr :>: Expr
                | Expr :=: Expr

infix 3 :<:
infix 3 :>:
infix 3 :=:


instance Num Params where
    (V x) + (V y) = V $ M.unionWith (+) x y
    (S x) + (S y) = S $ x+y
    _ + _ = error "adding scalar to vector"

    (V x) - (V y) = V $ M.unionWith (-) x y
    (S x) - (S y) = S $ x - y
    _ - _ = error "substracting scalar to vector"

    (V x) * (V y) = V $ M.unionWith (*) x y
    (S x) * (V y) = V $ fmap (*x) y
    (V x) * (S y) = V $ fmap (*y) x
    (S x) * (S y) = S $ x * y

    fromInteger x = S $ fromInteger x

    abs (S x) = S $ abs x
    abs (V x) = V $ fmap abs x

    signum _ = error "signum not implemented"


norm :: Values -> Double
norm v = sqrt $ sum $ zipWith (*) (M.elems v) (M.elems v)

gradientDescent :: Double -> Double -> Expr -> Values -> IO Values 
gradientDescent eps alpha expr vals 
        | norm grad < eps = do
            putStrLn $ "[minimum reached] "
            putStrLn $ "expression: f(x)=" ++ show expr
            putStrLn $ "f(x)=" ++ show loss 
            putStrLn $ "grad=" ++ show (norm grad) 
            putStrLn $ "vals=" ++ show (M.elems vals)
            pure vals
        | otherwise = do
            putStrLn $ "f(x)=" ++ show loss ++ " grad=" ++ show (norm grad) ++ " vals=" ++ showvals vals
            gradientDescent eps alpha expr vals'
    where (loss, grad) = evaluate expr vals
          (V vals') = V vals - lr * V grad 

          lr = S alpha


showvals :: Values -> String
showvals vals = unwords $ showvals' <$> M.assocs vals
    where showvals' (k,v) = show k ++ "=" ++ show v


{- Diverge :( -}

gradientDescentConstrained :: Double -> Double -> Expr -> [Constraint] -> Values -> IO Values
gradientDescentConstrained eps alpha expr ctrs vals = do
        putStrLn $ "[gradient] expression to be minimized: " ++ show expr 
        putStrLn $ "[gradient] lagrangian: " ++ show lagrangien
        putStrLn $ "[gradient] starting point: " ++ show newvals
        gradientDescent eps alpha lagrangien newvals
    
    where freeindices = (Var <$> [1..]) L.\\ M.keys vals 
          ctrexprs = ctrs >>= ctrToExpr
          lambdas = take (length ctrexprs) freeindices
          lambdaszeros = M.fromList $ zip lambdas $ repeat 0
          lagrangien = expr + sum (zipWith (*) lambdas ctrexprs)
          newvals = M.union vals lambdaszeros

ctrToExpr :: Constraint -> [Expr]
ctrToExpr (e :<: e') = [e - e']
ctrToExpr (e :>: e') = ctrToExpr (e' :<: e)
ctrToExpr (e :=: e') = concatMap ctrToExpr [e :<: e', e':>: e] -- TODO: test wiki
