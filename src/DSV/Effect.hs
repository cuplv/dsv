{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module DSV.Effect where

import Language.SMTLib2 hiding (Model,store)

import DSV.Logic

class DataModel d where
  model :: (Backend b) => SMT b (d b)

class (DataModel c) => Constrained c where
  constraint :: (Backend b) => Pr b (c b)

class (DataModel (Store e), Constrained (Param e)) => Effect e where
  type Store e :: * -> *
  type Param e :: * -> *

  allEffects :: [e]

  -- * SMT model of the parameter to an effect
  param :: (Backend b) => e -> SMT b (Param e b)
  wp    :: (Backend b) => e -> Param e b -> Pr b (Store e b)
  eff   :: (Backend b) => e -> Param e b -> Mod b (Store e b)

-- Instances

data UnitM b = UnitM

instance DataModel UnitM where
  model = return UnitM

data UnitC m b = UnitC (m b)

instance (DataModel m) => DataModel (UnitC m) where
  model = UnitC <$> model

instance (DataModel m) => Constrained (UnitC m) where
  constraint = const true

data BoolM b = BoolM (Expr b BoolType)

instance DataModel BoolM where
  model = BoolM <$> declareVar bool

data IntM b = IntM (Expr b IntType)

instance DataModel IntM where
  model = IntM <$> declareVar int

nonNegative :: (Backend b) => Pr b (IntM b)
nonNegative (IntM a) = a .>=. cint 0

newtype PosInt b = PosInt (IntM b)

instance DataModel PosInt where
  model = PosInt <$> model

positive :: (Backend b) => Pr b (PosInt b)
positive (PosInt (IntM a)) = a .>=. cint 1

instance Constrained PosInt where
  constraint = positive
