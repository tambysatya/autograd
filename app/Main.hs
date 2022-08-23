module Main (main) where

import BackProp
import Expr
import qualified Data.Map as M
import Optimization

--test = (2 * 4 + Var 1)*Var 2 + Var 3 + (Var 2)*(Var 1) + (Var 2* Var 1)

mkValues :: [Double] -> Values
mkValues = M.fromList . zip (Var <$> [1..])
mkVars :: Int -> [Expr]
mkVars p = Var <$> [1..p]

testVal :: Values
testVal = M.fromList [(Var 1,5),(Var 2,2), (Var 3,4)]
test3 = e + e*e
    where e = Var 1 * Var 2
test3Val :: Values    
test3Val = M.fromList [(Var 1,1), (Var 2,1)]

testexpr = log (5*x-2*y*z) + 7 * sqrt ( x*z / y) + cos (x/y)
    where [x,y,z] = Var <$> [1..3]

test = evaluate testexpr $ mkValues [20,1,1]


testOptExpr = (2*x + 3*y + 5*z)^2 + 1/x * (- y^2) 
    where [x,y,z] = mkVars 3
testOpt = gradientDescent (1e-10) 0.001 testOptExpr vals
    where vals = mkValues [10,10,10]


main :: IO ()
main = pure ()
