module Dual (DualNum, autodiff) where

import qualified Data.Array as A
import Control.Lens.At
import Control.Lens

{-| 
We consider numbers in the form x=a + bε.
ε is a constant such that ε² = 0
Similarily to complex numbers, usual operations are availables.
At the end of calculus, we have f(x) = f(a) +f'(a)ε.
Using the taylor serie of f, this result can be proven easily. Indeed:

f(x)=f(a+ε)= f(a)+εf'(a) + o(ε²)

Since ε²= 0, we have the result.
 
In this code, we implement this algebra il haskell.

-}    

{-| The number a + bε is represented as a :+: b -}
data DualNum = Double :+: Double 
    deriving Show

{-|  Extracts the coefficient of ε-}
_dual (_ :+: x') = x'
dual = lens (\(_ :+: x') -> x') (\(x :+: _) x' -> x :+: x')

infix 4 :+: 


{- Implementation of the algebra
In the following instances, u and v denotes the values of the derivative of a and b respectively-}
instance Num DualNum where
    (a :+: u) + (b :+: v) = a + b :+: u + v
    (a :+: u) * (b :+: v) = (a*b) :+: a*v + b*u
    negate (a :+: u) = negate a :+: negate u
    abs _ = error "abs is not differentiable"
    signum _ = error "signum is not differentiable"

    {- Typing a raw number like 3 :: DualNum will
       create a constant (the imaginary part is
       equal to zero).
       Variables must be specified explicitely
       (by setting their imaginary part to 1)
    -}
    fromInteger x = fromInteger x :+: 0


instance Fractional DualNum where
    (a :+: u) / (b :+: v)
        | b == 0 = error "division by zero"
        | otherwise = a / b :+: (u*b - a*v)/b^2
    fromRational f = fromRational f :+: 0

instance Floating DualNum where
    pi = pi :+: 0
    exp (a :+: u) = exp a :+: u*exp(a)
    log (a :+: u) 
        | a > 0 = log a :+: u / a
        | otherwise = error "log on negative number"
    sin (a :+: u) = sin a :+: u* cos a
    cos (a :+: u) = cos a :+: negate (u * sin a)
    asin (a :+: u) 
        | a < 1 && a > -1 = asin a :+: u / sqrt (1 - a^2)
        | otherwise = error "asin: out of range" 
    acos (a :+: u) 
        | a < 1 && a > -1 = acos a :+: negate (u / sqrt (1-a^2))
        | otherwise = error "asin: out of range" 
    atan (a :+: u) = atan a :+: u / (1 + a^2)
    cosh x = (exp x + exp (-x))/2
    sinh x = (exp x - exp (-x))/2
    asinh x = log $ x + sqrt(x^2+1)
    acosh x = log $ x + sqrt(x^2-1)
    atanh x = 1/2 * log((1+x)/(1-x))


{- From here, the computation of the derivative can be performed.
-} 
{-| Computes the derivative of f-}    
autodiff :: (DualNum -> DualNum) -> Double -> Double
autodiff f x  = grad $ f (x :+: 1)
    where grad (_ :+: g) = g

{-| Computes the gradient of f
  - We set all the other variables to constants (by setting their imaginary part to 0)
  - We set x to be a variable (by setting its imaginary part to 1)

-}
autograd :: (A.Array Int DualNum -> DualNum) -> A.Array Int Double -> A.Array Int Double
autograd f x = A.listArray (1,n) $ fmap _dual [f $ xdual & ix i . dual .~ 1 | i <- [1..n]]
    where (_,n) = A.bounds x
          xdual = fromRational . toRational <$> x 

{-| Computes the jacobian of f
Since the function outputs a vector, we need performs as previously,
but at each iteration, we collect the derivatives relative to each output
-}
autojacobian :: (A.Array Int DualNum -> A.Array Int DualNum) -> A.Array Int Double -> A.Array (Int,Int) Double
autojacobian f x = fmap _dual $ A.array ((1,1),(m,n)) [((i,j),f (entry j) A.! i) | i <- [1..m], j <- [1..n]]
    where 
          (_,m) = A.bounds $ f x_dual
          (_,n) = A.bounds x
          x_dual = fromRational . toRational <$> x
          entry i = x_dual & ix i . dual .~ 1

{- Utils -}
mkArray l = A.listArray (1,length l) l

showMatrix mat = unlines [unwords [show $ mat A.! (i,j) | j <- [1..n]] | i <- [1..m]]
    where (_,(m,n)) = A.bounds mat

{- Test -}
testFun vars = mkArray [exp (2*x + 3*y) - 2*z
                      , 2*x + 3*y]
    where [x,y,z] = A.elems vars

test = autojacobian testFun $ mkArray [2,1,5]
