{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module DSV.Examples.Bank where

import Language.SMTLib2 hiding (Model,store)

import DSV.Logic
import DSV.Effect
import DSV.Contract

data Bank = Wd | Dp deriving (Show,Eq,Ord)

instance Effect Bank where
  type Store Bank = IntM
  type Param Bank = PosInt

  allEffects = [Wd,Dp]

  param _ = model
  wp Wd = \(PosInt (IntM n)) (IntM a) -> (a .>=. n)
  wp Dp = \_ _ -> true
  eff Wd = \(PosInt (IntM n)) (IntM a) -> IntM <$> (a .-. n)
  eff Dp = \(PosInt (IntM n)) (IntM a) -> IntM <$> (a .+. n)

data BadBank = Bad Bank deriving (Show,Eq,Ord)

instance Effect BadBank where
  type Store BadBank = Store Bank
  type Param BadBank = Param Bank
  
  allEffects = map Bad allEffects

  param (Bad e) = param e
  wp (Bad Wd) = \_ _ -> true
  wp (Bad e) = wp e
  eff (Bad e) = eff e

bankRules (Wd,Wd) = True
bankRules _ = False

badBankRules (Bad Wd,Bad Wd) = True
badBankRules _ = False

bankC :: Contract Bank
bankC = Vis bankRules

badBankC :: Contract BadBank
badBankC = Vis badBankRules
