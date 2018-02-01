{-# LANGUAGE DataKinds #-}

module DSV.Model
  ( DataModel (..)
  ) where

import Language.SMTLib2

-- | A datatype which can be universally quantified as a structure of
--   SMT variables
class DataModel d where

  -- | Produce SMT variables representing the universal quantification
  --   of this datatype
  model :: (Backend b) => SMT b (d b)
  modEq :: (Backend b) => d b -> d b -> SMT b (Expr b BoolType)
