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
testOpt = gradientDescent (1e-5) 0.01 testOptExpr vals
    where vals = mkValues [10,10,10]

ctrs :: [Constraint]
ctrs = [--2*x + y + z :=: 5,
        --x + z :=: 10,
        --2*x + z :=: 1
        -- x :>: 0,
        -- y :>: 0,
        -- z :>: 0]
        x :=: 0,
        y :=: 0,
        z :=: 0 ]
    where [x,y,z] = mkVars 3

ctrs2 = [x :>: 0, y :>: 0, z :>: 0]
    where [x,y,z] = mkVars 3
ctrs3 = [0 :>: x, 0 :>: y, 0 :>: z]
    where [x,y,z] = mkVars 3
ctrs4 = [x + 2*y + z :=: 5]
    
[x,y,z] = mkVars 3
testConvexExpr = (x + 2*y + z)^2
    where [x,y,z] = mkVars 3
testAugmentedExpr = (x + 2*y + z)^2 + x*l1 
    where [x,y,z,l1,l2,l3] = mkVars 6


main :: IO ()
main = pure ()
