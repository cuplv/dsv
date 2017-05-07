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
