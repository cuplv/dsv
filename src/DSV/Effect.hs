{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module DSV.Effect 
  ( DataModel (..)
  , Constrained (..)
  , Effect (..)
  ) where

import Language.SMTLib2

import DSV.Logic

-- | A datatype which can be universally quantified as a structure of
--   SMT variables
class DataModel d where

  -- | Produce SMT variables representing the universal quantification
  --   of this datatype
  model :: (Backend b) => SMT b (d b)

-- | A 'DataModel' which has constraint expressions on its variables
class (DataModel c) => Constrained c where

  -- | A logical predicate on the SMT variables modeling 'c' which
  --   expresses the constraints on those variables
  constraint :: (Backend b) => Pr b (c b)

-- | A datatype which represents effects in a distributed data-store
-- program
class (DataModel (Store e), Constrained (Param e)) => Effect e where
  -- | The datatype modeling the distributed store on which the
  --   effects are applied
  type Store e :: * -> *
  -- | The datatype modeling parameters to the effects
  type Param e :: * -> *

  -- | List all forms of effects
  allEffects :: [e]

  -- | Produce a parameter model for a given effect form
  param :: (Backend b) => e -> SMT b (Param e b)
  
  -- | Produce an expression representing the weakest precondition of
  --   a particular parameterized effect being emitted
  wp    :: (Backend b) => e -> Param e b -> Pr b (Store e b)

  -- | Produce a transformer representing the concrete change to the
  --   store of a particular parameterized effect
  eff   :: (Backend b) => e -> Param e b -> Mod b (Store e b)
