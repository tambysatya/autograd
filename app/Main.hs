module Main (main) where

import BackProp
import Expr
import qualified Data.Map as M

--test = (2 * 4 + Var 1)*Var 2 + Var 3 + (Var 2)*(Var 1) + (Var 2* Var 1)

mkValues :: [Double] -> Values
mkValues = M.fromList . zip (Var <$> [1..])

testVal :: Values
testVal = M.fromList [(Var 1,5),(Var 2,2), (Var 3,4)]
test3 = e + e*e
    where e = Var 1 * Var 2
test3Val :: Values    
test3Val = M.fromList [(Var 1,1), (Var 2,1)]

testexpr = log (5*x-2*y*z) + 7 * sqrt ( x*z / y) + cos (x/y)
    where [x,y,z] = Var <$> [1..3]

test = evaluate testexpr $ mkValues [20,1,1]

main :: IO ()
main = pure ()
