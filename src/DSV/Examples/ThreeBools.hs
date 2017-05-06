{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeOperators #-}

module DSV.Examples.ThreeBools where

import Language.SMTLib2 hiding (Model,store)

import DSV.Logic
import DSV.Effect
import DSV.Contract

data ThreeBoolsS = ThreeBoolsS

instance DValue ThreeBoolsS where
  data Model ThreeBoolsS b = ThreeBoolsModel (Expr b BoolType)
                                             (Expr b BoolType)
                                             (Expr b BoolType)
  model _ = ThreeBoolsModel 
            <$> (declareVar bool)
            <*> (declareVar bool)
            <*> (declareVar bool)

data ThreeBools = I2S1 | I3S2 | I1S3 deriving (Show,Eq,Ord)

instance DValue () where
  data Model () b = UnitModel
  model _ = return UnitModel

instance Parameter () where
  constraint = const true

instance Effect ThreeBools where
  type Store ThreeBools = ThreeBoolsS
  type Param ThreeBools = ()
  
  allEffects = [I2S1,I3S2,I1S3]
  store _ = model ThreeBoolsS
  
  param _ = model ()
  wp I2S1 = \_ (ThreeBoolsModel b1 b2 b3) -> not' b2
  wp I3S2 = \_ (ThreeBoolsModel b1 b2 b3) -> not' b3
  wp I1S3 = \_ (ThreeBoolsModel b1 b2 b3) -> not' b1
  eff I2S1 = \_ (ThreeBoolsModel b1 b2 b3) -> 
               ThreeBoolsModel <$> true <*> pure b2 <*> pure b3
  eff I3S2 = \_ (ThreeBoolsModel b1 b2 b3) -> 
               ThreeBoolsModel <$> pure b1 <*> true <*> pure b3
  eff I1S3 = \_ (ThreeBoolsModel b1 b2 b3) -> 
               ThreeBoolsModel <$> pure b1 <*> pure b2 <*> true



notAllBools :: (Backend b) => Pr b (Model (Store ThreeBools) b)
notAllBools = \(ThreeBoolsModel b1 b2 b3) -> not' (b1 .&. b2 .&. b3)
