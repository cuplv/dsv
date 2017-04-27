module DSV.Effect.Bank where

import Z3.Monad

import DSV.Logic
import DSV.Effect

data Bank = Wd | Dp deriving (Show,Eq,Ord)

zero :: (MonadZ3 m) => m AST
zero = mkInteger 0

one :: (MonadZ3 m) => m AST
one = mkInteger 1

instance Effect Bank where
  allEffects = [Wd,Dp]

  wp Wd v = mkGe v =<< one
  wp Dp _ = mkTrue
  
  eff e a = do one' <- one
               (case e of
                  Wd -> mkSub
                  Dp -> mkAdd) [a,one']

  cvis Wd Wd = True
  cvis _ _ = False

nonNegative :: Pred
nonNegative a = mkGe a =<< zero
