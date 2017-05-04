{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module DSV.Effect.Bank where

import Language.SMTLib2 hiding (Model,store)

import DSV.Logic
import DSV.Effect
import DSV.Contract

data IntV = IntV

instance DValue IntV where
  data Model IntV b = IntVModel (Expr b IntType)
  model _ = IntVModel <$> declareVar int

instance Parameter IntV where
  constraint = nonNegative

data Bank = Wd | Dp deriving (Show,Eq,Ord)

instance Effect Bank where
  type Store Bank = IntV
  type Param Bank = IntV
                    
  allEffects = [Wd,Dp]
  store _ = model IntV

  param _ = model IntV
  wp Wd = \(IntVModel n) (IntVModel a) -> (a .>=. n)
  wp Dp = \_ _ -> true
  eff Wd = \(IntVModel n) (IntVModel a) -> IntVModel <$> (a .-. n)
  eff Dp = \(IntVModel n) (IntVModel a) -> IntVModel <$> (a .+. n)

data BadBank = Bad Bank deriving (Show,Eq,Ord)

instance Effect BadBank where
  type Store BadBank = Store Bank
  type Param BadBank = Param Bank
  
  allEffects = map Bad allEffects
  store (Bad e) = store e

  param (Bad e) = param e
  wp (Bad Wd) = \_ _ -> true
  wp (Bad e) = wp e
  eff (Bad e) = eff e

nonNegative :: (Backend b) => Pr b (Model (Store Bank) b) -- Pr b (Expr b IntType)
nonNegative = \(IntVModel a) -> a .>=. cint 0

bankRules (Wd,Wd) = True
bankRules _ = False

badBankRules (Bad Wd,Bad Wd) = True
badBankRules _ = False

bankC :: Contract Bank
bankC = Vis bankRules

badBankC :: Contract BadBank
badBankC = Vis badBankRules
