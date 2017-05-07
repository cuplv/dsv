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

-- | An empty value containing no information
data UnitM b = UnitM

instance DataModel UnitM where
  model = return UnitM

-- | An empty constraint that allows anything
--
--   Programs without effect parameters will use @('UnitC' 'UnitM')@ as
--   \"the empty parameter\" for their 'Param' type.
data UnitC m b = UnitC (m b)

instance (DataModel m) => DataModel (UnitC m) where
  model = UnitC <$> model

instance (DataModel m) => Constrained (UnitC m) where
  constraint = const true

-- | A boolean model type, represented as a single 'BoolType' SMT
--   expression
data BoolM b = BoolM (Expr b BoolType)

instance DataModel BoolM where
  model = BoolM <$> declareVar bool

-- | An integer model type, represented as a single 'IntType' SMT
--   expression
data IntM b = IntM (Expr b IntType)

instance DataModel IntM where
  model = IntM <$> declareVar int

-- | A predicate stating that an 'IntM' be non-negative
nonNegative :: (Backend b) => Pr b (IntM b)
nonNegative (IntM a) = a .>=. cint 0

-- | a predicate stating that an 'IntM' be greater than zero
positive :: (Backend b) => Pr b (IntM b)
positive (IntM a) = a .>=. cint 1

-- | A constrained 'IntM' that is positive
newtype PosInt b = PosInt (IntM b)

instance DataModel PosInt where
  model = PosInt <$> model

instance Constrained PosInt where
  constraint = \(PosInt i) -> positive i
