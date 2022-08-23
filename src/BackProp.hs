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

type BPState a = StateT BP IO a

addParent :: Expr -> Expr -> BPState ()
addParent parent child = do
    entryM <- use $ graph . at child
    case entryM of
        Nothing -> graph . at child .= Just [parent]
        Just l -> when (not $ parent `elem` l) $ graph . at child . _Just %= (parent:)

delParent :: Expr -> Expr -> BPState ()
delParent parent child = graph . at child . _Just %= L.delete parent
    

forward :: Expr -> BPState Double
forward expr = do
    valueM <- use $ values . at expr
    liftIO $ putStrLn $ "\t[evaluate] " ++ show expr 
    adjoints . at expr .= Just 0
    ret <- case valueM of
            Just val -> do liftIO $ putStrLn $ "\t[evaluate] found " ++ show expr ++ "=" ++ show val
                           pure val
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




addAdjoint :: Expr -> Double -> BPState ()
addAdjoint expr val = adjoints . at expr . _Just %= (+val)



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
                Just (x:xs) -> do
                    liftIO $ putStrLn $ "\t[backprop] trying " ++ show expr ++ " [backtrack] -> " ++ show x
                    backprop x
                Just [] -> do 
                    (Just adj) <- use $ adjoints . at expr
                    liftIO $ putStrLn $ "\t[backprop] " ++ show expr ++ " has derivative " ++ show adj
                    gradients . at expr .= Just adj
                    case expr of
                        Var v -> pure ()
                        Op op -> do
                            let (e,e') = operands op
                            (Just x, Just y) <- (,) <$> use (values . at e) <*> use (values . at e')
                            let (adje,adje') = diffOp op x y adj
                            addAdjoint e adje >> addAdjoint e' adje'
                            delParent expr e >> delParent expr e'
                            backprop e >> backprop e'
                        Fun f -> do
                            let e = variable f
                            (Just x) <- use $ values . at e
                            let adje = diffFun f x adj
                            addAdjoint e adje
                            delParent expr e
                            backprop e

evaluate :: Expr -> Values -> IO (Double, Values)
evaluate expr vals = do
    (ret, forwardpass) <- runStateT (forward expr) $ BP M.empty vals M.empty  M.empty
    putStrLn $ "evaluate has returned: " ++ show ret

    -- Tous les gradients sont à 0 excepté pour celui de l'expression
    (_,backwardpass) <- runStateT (forM (M.keys vals) backprop) $ forwardpass & adjoints . at expr .~ Just 1
    putStrLn $ "computing graph of : f(x)=" ++ show expr
    dumpGraph (_graph forwardpass)
    let grads = fromJust <$> M.fromList [(xi, xi `M.lookup` _adjoints backwardpass) | xi <- M.keys vals]
    forM (M.keys vals) $ \xi -> putStrLn $ "grad_" ++ show xi ++ "=" ++ show (fromJust $ xi `M.lookup` _adjoints backwardpass)
    --putStrLn $ unlines $ show <$> M.assocs ( _adjoints grad)

    pure (ret,grads)

dumpGraph :: ReverseGraph -> IO ()
dumpGraph gr = forM_ (M.assocs gr) (putStrLn . dumpEntry)
    where   dumpEntry :: (Expr, [Expr]) -> String
            dumpEntry (k,e) = show k ++ " appears in:\n " ++ unlines ( ('\t':) . show <$> e)
