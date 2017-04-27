module DSV.Effect.Bank where

import Z3.Monad

import DSV.Logic
import DSV.Effect
import DSV.Contract

zero :: (MonadZ3 m) => m AST
zero = mkInteger 0

one :: (MonadZ3 m) => m AST
one = mkInteger 1

data Bank = Wd | Dp deriving (Show,Eq,Ord)

instance Effect Bank where
  allEffects = [Wd,Dp]

  wp Wd v = mkGe v =<< one
  wp Dp _ = mkTrue
  
  eff e a = do one' <- one
               (case e of
                  Wd -> mkSub
                  Dp -> mkAdd) [a,one']

data BadBank = Bad Bank deriving (Show,Eq,Ord)

instance Effect BadBank where
  allEffects = map Bad allEffects
  
  wp (Bad Wd) v = mkTrue -- remove WP from withdraws
  wp (Bad e) v = wp e v -- keep others the same
  
  eff (Bad e) a = eff e a

nonNegative :: Pred
nonNegative a = mkGe a =<< zero

bankRules (Wd,Wd) = True
bankRules _ = False

badBankRules (Bad Wd,Bad Wd) = True
badBankRules _ = False

bankC :: Contract Bank
bankC = Vis bankRules

badBankC :: Contract BadBank
badBankC = Vis badBankRules
