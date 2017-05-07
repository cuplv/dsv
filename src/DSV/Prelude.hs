{-# LANGUAGE DataKinds #-}

module DSV.Prelude 
  ( UnitM (..)
  , UnitC (..)
  , BoolM (..)
  , IntM (..)
  , nonNegative
  , positive
  , PosInt (..)
  , module DSV
  ) where
  
import Language.SMTLib2

import DSV

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
