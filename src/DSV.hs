module DSV where

import Z3.Monad
import Control.Monad.Trans (liftIO)
import Data.Maybe

import DSV.Logic
import DSV.Effect
import DSV.Contract
import DSV.Effect.Bank

-- * Verify a proposition by checking for UNSAT of its negation
verify :: Z3 AST -> IO Bool
verify p = interp <$> evalZ3 (p >>= mkNot >>= assert >> check)
  where interp Unsat = True
        interp _ = False

-- * Check that a program is safe, given a contract and invariant
program :: (Effect e) => [e] -> Contract e -> Pred -> Z3 AST
program es c i = mkAnd =<< mapM (consafe c i) es

safe :: (Effect e) => Pred -> e -> Z3 AST
safe i e = prePost (i,i) (eff e)

seqsafe :: (Effect e) => Pred -> e -> Z3 AST
seqsafe i e = do let pre a = mkAnd =<< sequence [i a,wp e a]
                 prePost (pre,i) (eff e)

strong :: (Effect e) => Contract e -> (e,e) -> Z3 AST
strong c (e0,e1) = if vis c (e0,e1)
                      then mkTrue
                      else mkFalse

comp :: (Effect e) => Contract e -> Pred -> (e,e) -> Z3 AST
comp c i (e0,e1) = do safe' <- safe (wp e1) e0
                      strong' <- strong c (e0,e1)
                      mkOr [strong',safe']

consafe :: (Effect e) => Contract e -> Pred -> e -> Z3 AST
consafe c i e = do comps <- mapM (\e0 -> comp c i (e0,e)) allEffects
                   seqsafe' <- seqsafe i e
                   mkAnd (seqsafe' : comps)
