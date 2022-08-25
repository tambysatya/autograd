{-# LANGUAGE TemplateHaskell #-}
module BackProp where

import Expr

import qualified Data.Map as M
import qualified Data.Array as A
import qualified Data.List as L

import Control.Monad.State
import Control.Lens
import Data.Maybe



type ReverseGraph = M.Map Expr [Expr]
type Values = M.Map Expr Double
type Adjoints = M.Map Expr Double
type GradMap = M.Map Expr Double


data BP = BP {_graph :: ReverseGraph, _values :: Values, _adjoints :: Adjoints, _gradients :: GradMap}

makeLenses ''BP

type BPState a = State BP a

addParent :: Expr -> Expr -> BPState ()
addParent parent child = do
    entryM <- use $ graph . at child
    case entryM of
        Nothing -> graph . at child .= Just [parent]
        Just l -> when (not $ parent `elem` l) $ graph . at child . _Just %= (parent:)

delParent :: Expr -> Expr -> BPState ()
delParent parent child = graph . at child . _Just %= L.delete parent
addAdjoint :: Expr -> Double -> BPState ()
addAdjoint expr val = adjoints . at expr . _Just %= (+val)


    

forward :: Expr -> BPState Double
forward expr = do
    valueM <- use $ values . at expr
    adjoints . at expr .= Just 0
    ret <- case valueM of
            Just val -> pure val
            Nothing -> do
                graph . at expr .= Just []
                case expr of
                    Constant x -> pure x
                    Var idx -> error $ "Variable " ++ show idx ++ " has no value"
                    Op op -> do 
                        let (e,e') = operands op
                        (x,y) <- (,) <$> forward e <*> forward e'
                        addParent expr e >> addParent expr e'
                        pure $ evalOp op x y
                    Fun f -> do 
                        let e = variable f
                        x <- forward e
                        addParent expr e
                        pure $ evalFun f x
                    
    values . at expr .= Just ret
    pure ret


forwardNoGrad :: Expr -> BPState Double
forwardNoGrad expr = do
    valueM <- use $ values . at expr
    ret <- case valueM of
            Just val -> pure val
            Nothing -> do
                graph . at expr .= Just []
                case expr of
                    Constant x -> pure x
                    Var idx -> error $ "Variable " ++ show idx ++ " has no value"
                    Op op -> do 
                        let (e,e') = operands op
                        (x,y) <- (,) <$> forwardNoGrad e <*> forwardNoGrad e'
                        pure $ evalOp op x y
                    Fun f -> do 
                        let e = variable f
                        x <- forwardNoGrad e
                        pure $ evalFun f x
                    
    values . at expr .= Just ret
    pure ret



backprop :: Expr -> BPState ()
backprop (Constant _) = pure ()
backprop expr = do
    grad <- use $ gradients . at expr
    case grad of
        Just _ -> pure ()
        Nothing -> do 
            parentsM <- use $ graph . at expr
            case parentsM of
                Nothing -> error $ show expr ++ " is not in the graph"
                Just (x:xs) -> backprop x
                Just [] -> do 
                    adj <- fromJust <$> (use $ adjoints . at expr)
                    gradients . at expr .= Just adj
                    case expr of
                        Var v -> pure ()
                        Op op -> do
                            let (e,e') = operands op
                            (x,y) <- (over both fromJust) <$> ((,)  <$> use (values . at e) <*> use (values . at e'))
                            let (adje,adje') = diffOp op x y adj
                            addAdjoint e adje >> addAdjoint e' adje'
                            delParent expr e >> delParent expr e'
                            backprop e >> backprop e'
                        Fun f -> do
                            let e = variable f
                            x <- fromJust <$> (use $ values . at e)
                            let adje = diffFun f x adj
                            addAdjoint e adje
                            delParent expr e
                            backprop e

computeGrad :: Expr -> BPState (Double, Values)
computeGrad expr = do
    vals <- use values
    ret <- forward expr 
    adjoints . at expr .= Just 1
    forM (M.keys vals) backprop
    grads <- forM (M.keys vals) $ \vi -> fromJust <$> use (adjoints . at vi)
    pure (ret, M.fromList $ zip (M.keys vals) grads)

evaluate :: Expr -> Values -> (Double, Values)
evaluate expr vals = fst $ runState (computeGrad expr) $ BP M.empty vals M.empty M.empty
evaluateNoGrad expr vals = fst $ runState (forwardNoGrad expr) $ BP M.empty vals M.empty M.empty



{- Utils -}
dumpGraph :: ReverseGraph -> IO ()
dumpGraph gr = forM_ (M.assocs gr) (putStrLn . dumpEntry)
    where   dumpEntry :: (Expr, [Expr]) -> String
            dumpEntry (k,e) = show k ++ " appears in:\n " ++ unlines ( ('\t':) . show <$> e)
