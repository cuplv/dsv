module DSV where

import Z3.Monad
import Control.Monad.Trans (liftIO)
import Data.Maybe

import DSV.Logic
import DSV.Effect
import DSV.Effect.Bank

safe :: (Effect e) => Pred -> e -> Z3 AST
safe i e = prePost (i,i) (eff e)

seqsafe :: (Effect e) => Pred -> e -> Z3 AST
seqsafe i e = prePost (wp e,i) (eff e)

strong :: (Effect e) => (e,e) -> Z3 AST
strong (e0,e1) = if cvis e0 e1
                    then mkTrue
                    else mkFalse

comp :: (Effect e) => Pred -> (e,e) -> Z3 AST
comp i (e0,e1) = do safe' <- safe (wp e1) e0
                    strong' <- strong (e0,e1)
                    mkOr [strong',safe']

consafe :: (Effect e) => Pred -> e -> Z3 AST
consafe i e = mkAnd =<< mapM (\e0 -> comp i (e0,e)) allEffects

script2 :: Z3 Result
script2 = do -- assert =<< mkNot =<< consafe nonNegative Dp
             -- assert =<< mkNot =<< consafe nonNegative Wd
             dp' <- mkNot =<< consafe nonNegative Dp
             wd' <- mkNot =<< consafe nonNegative Wd
             assert =<< mkOr [dp',wd']
             -- assert =<< mkNot =<< comp nonNegative (Dp,Wd)
             -- assert =<< mkNot =<< comp nonNegative (Dp,Dp)
             -- assert =<< mkNot =<< comp nonNegative (Wd,Dp)
             check

script :: Z3 (Maybe [Integer])
script = do a <- mkFreshIntVar "a"
            zero <- mkInteger 0
            let i = (\a -> mkNot =<< mkGe a zero) :: AST -> Z3 AST
            precond <- mkGe a zero
            fails <- mkOr =<< (sequence [i =<< (eff Wd) a
                                        ,i =<< (eff Dp) a])
            assert precond
            assert fails
            fmap snd $ withModel $ \m -> 
              catMaybes <$> mapM (evalInt m) [a]

opts = opt "MODEL" True

runTest :: Z3 (Maybe [Integer]) -> IO ()
runTest t = evalZ3With Nothing opts t >>= \mbSol -> 
              case mbSol of
                Nothing -> error "No solution found."
                Just sol -> putStr "Solution: " >> print sol

dsv = undefined
