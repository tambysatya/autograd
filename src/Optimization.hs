module Optimization where


import Expr
import BackProp
import qualified Data.Map as M
import qualified Data.List as L
import Debug.Trace


data Params = V Values
            | S Double


instance Show Params where
    show (V x) = show x
    show (S x) = show x

data Constraint = Expr :<: Expr
                | Expr :>: Expr
                | Expr :=: Expr
                | Zero Expr

infix 3 :<:
infix 3 :>:
infix 3 :=:


instance Num Params where
    (V x) + (V y) = V $ M.unionWith (+) x y
    (S x) + (S y) = S $ x+y
    (S x) + (V y) = V $ fmap (+x) y
    (V y) + (S x) = V $ fmap (+x) y

    (V x) - (V y) = V $ M.unionWith (-) x y
    (S x) - (S y) = S $ x - y
    _ - _ = error "substracting scalar to vector"

    -- elementwise mult
    (V x) * (V y) = V $ M.unionWith (*) x y
    (S x) * (V y) = V $ fmap (*x) y
    (V x) * (S y) = V $ fmap (*y) x
    (S x) * (S y) = S $ x * y

    fromInteger x = S $ fromInteger x

    abs (S x) = S $ abs x
    abs (V x) = V $ fmap abs x

    signum _ = error "signum not implemented"
instance Fractional Params where
    (S x) / (S y) = S $ x / y
    (V x) / (S y) = V $ fmap (/y) x
    (V x) / (V y) = V $ M.unionWith (/) x y
instance Floating Params where
    sqrt (S x) = S $ sqrt x
    sqrt (V x) = V $ fmap sqrt x

type Solver = Double -> Expr -> Values -> Values

norm :: Values -> Double
norm v = sqrt $ sum $ zipWith (*) (M.elems v) (M.elems v)

dotProduct :: Values -> Values -> Params
dotProduct v v' = S $ sum $ zipWith (*) (M.elems v) (M.elems v')

zeroes :: Values -> Values
zeroes = fmap (pure 0)

gradientDescentVerbose :: Double -> Double -> Expr -> Values -> IO Values 
gradientDescentVerbose eps alpha expr vals 
        | norm grad < eps = do
            putStrLn $ "[minimum reached] "
            putStrLn $ "expression: f(x)=" ++ show expr
            putStrLn $ "f(x)=" ++ show loss 
            putStrLn $ "grad=" ++ show (norm grad) 
            putStrLn $ "vals=" ++ show (M.elems vals)
            pure vals
        | otherwise = do
            putStrLn $ "f(x)=" ++ show loss ++ " grad=" ++ show (norm grad) ++ " vals=" ++ showvals vals
            gradientDescentVerbose eps alpha expr vals'
    where (loss, grad) = evaluate expr vals
          (V vals') = V vals - lr * V grad 

          lr = S alpha


showvals :: Values -> String
showvals vals = unwords $ showvals' <$> M.assocs vals
    where showvals' (k,v) = show k ++ "=" ++ show v



gradientDescent :: Double -> Double -> Expr-> Values -> Values
gradientDescent tol lr lagrangian initvals = gradientDescent' 0 initvals
    where gradientDescent' it vals 
                    | norm gradloss < tol = vals
                    | otherwise = (if it `mod` 100 == 0 && it /= 0 then trace ("\t[descent] [" ++ show it ++ "] loss=" ++ show loss ++ " grad=" ++ show (norm gradloss)) else id)$ 
                                    gradientDescent' (it+1) newvals
            where (loss, gradloss) = evaluate lagrangian vals
                  (V newvals) = V vals - (S lr)*(V gradloss)
                                             

adam :: Double -> Solver
adam lr tol expr initvals = adam' 1 initvals (V $ zeroes initvals) (V $ zeroes initvals)
    where beta1 = S 0.9
          beta2 = S 0.999
          eps = S 1e-8
          adam' it values mt vt 
                    | norm grad < tol = values
                    | otherwise = (if it `mod` 100 == 0 then trace ("\t[adam] [" ++ show it ++ "] loss=" ++ show loss ++ " grad=" ++ show (norm grad)) else id) $ 
                                    adam' (it+1) newvals _newmt _newvt
    

                where (loss, grad) = evaluate expr values
                      _newmt = beta1 * mt + (1-beta1) * V grad
                      _newvt = beta2 * vt + (1-beta2) * (V grad)^2
                      newmt = _newmt / (1 - beta1^it)
                      newvt = _newvt / (1 - beta2^it)
                      (V newvals) = (V values) - S lr *newmt / (sqrt(newvt) + eps)
                      

augmentedLagrangian :: Solver -> Double -> Expr -> Constraint -> Expr -> Expr -> Values -> IO Values
augmentedLagrangian solver tol expr (Zero ctr) mu lambda initvals = do
        let xk = solver tol lagrangian initvals
        --let xk = gradientDescent tol lr lagrangian initvals
            (loss, ctrval) = (evaluateNoGrad expr xk, evaluateNoGrad ctr xk)
            nextmu = mu + 5
            nextlambda = lambda + mu* (Constant ctrval)
        if abs ctrval >= tol
            then do
                putStrLn $ "[augmented lagrangian] loss=" ++ show loss ++ " ctr=" ++ show ctrval ++ " mu=" ++ show mu
                augmentedLagrangian solver tol expr (Zero ctr) nextmu nextlambda xk
            else do
                putStrLn $ "[augmented lagrangian] constrained minimum found"
                putStrLn $ "\tloss=" ++ show loss
                putStrLn $ "\tctrs=" ++ show ctrval 
                putStrLn $ "\tvars=" ++ showvals xk
                pure xk

    where lagrangian = expr + mu/2 * ctr ^2 + lambda*ctr
    
