{-# LANGUAGE DataKinds #-}

module DSV.Effect.Bank where

import Language.SMTLib2

import DSV.Logic
import DSV.Effect
import DSV.Contract

data Bank = Wd | Dp deriving (Show,Eq,Ord)

instance Effect Bank where
  allEffects = [Wd,Dp]

  wp Wd = \a -> a .>=. cint 1
  wp Dp = const true

  eff Wd = \a -> a .-. cint 1
  eff Dp = \a -> a .+. cint 1

data BadBank = Bad Bank deriving (Show,Eq,Ord)

instance Effect BadBank where
  allEffects = map Bad allEffects
  
  wp (Bad Wd) = const true -- remove WP from withdraws
  wp (Bad e) = wp e -- keep others the same

  eff (Bad e) = eff e

nonNegative :: (Backend b) => Pr b IntType
nonNegative a = a .>=. cint 0

bankRules (Wd,Wd) = True
bankRules _ = False

badBankRules (Bad Wd,Bad Wd) = True
badBankRules _ = False

bankC :: Contract Bank
bankC = Vis bankRules

badBankC :: Contract BadBank
badBankC = Vis badBankRules
