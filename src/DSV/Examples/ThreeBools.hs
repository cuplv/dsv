{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}

module DSV.Examples.ThreeBools where

import Language.SMTLib2 hiding (Model,store)

import DSV.Logic
import DSV.Effect
import DSV.Contract

data B3 b = B3 { b1 :: Expr b BoolType
               , b2 :: Expr b BoolType
               , b3 :: Expr b BoolType }

notAllBools :: (Backend b) => Pr b (B3 b)
notAllBools (B3 b1 b2 b3) = not' (b1 .&. b2 .&. b3)

instance DataModel B3 where
  model = B3 <$> mkb <*> mkb <*> mkb
    where mkb = declareVar bool

data ThreeBools = S1 | S2 | S3 deriving (Show,Eq,Ord)

instance Effect ThreeBools where
  type Store ThreeBools = B3
  type Param ThreeBools = UnitC UnitM

  allEffects = [S1,S2,S3]

  param _ = model
  wp S1 = \_ -> not' . b2
  wp S2 = \_ -> not' . b3
  wp S3 = \_ -> not' . b1
  eff S1 = \_ (B3 b1 b2 b3) -> B3 <$> true <*> pure b2 <*> pure b3
  eff S2 = \_ (B3 b1 b2 b3) -> B3 <$> pure b1 <*> true <*> pure b3
  eff S3 = \_ (B3 b1 b2 b3) -> B3 <$> pure b1 <*> pure b2 <*> true



