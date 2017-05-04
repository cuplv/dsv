{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module DSV.Effect.Bank where

import Language.SMTLib2

import DSV.Logic
import DSV.Effect
import DSV.Contract

data Bank = Wd | Dp deriving (Show,Eq,Ord)
 
newtype Expr' t b = Expr' { unExpr' :: Expr b t}

instance Effect Bank where
  type Arg Bank = Expr' IntType
  type ArgM Bank = SMT
  allEffects = [Wd,Dp]

  wp Wd = \(Expr' n) a -> a .>=. n
  wp Dp = \(Expr' n) _ -> true

  eff Wd = \(Expr' n) a -> a .-. n
  eff Dp = \(Expr' n) a -> a .+. n
  
  arg _ = Expr' <$> declareVar int
  argc _ = \(Expr' n) -> nonNegative n

data BadBank = Bad Bank deriving (Show,Eq,Ord)

instance Effect BadBank where
  type Arg BadBank = Arg Bank
  type ArgM BadBank = ArgM Bank
  allEffects = map Bad allEffects
  
  wp (Bad Wd) = \_ _ -> true -- remove WP from withdraws
  wp (Bad e) = wp e -- keep others the same

  eff (Bad e) = eff e
  
  arg (Bad e) = arg e
  argc (Bad e) = argc e

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
