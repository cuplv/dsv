{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstrainedClassMethods #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}


module DSV.Effect where

import Language.SMTLib2 hiding (Model,store)

import DSV.Logic

class DValue d where
  data Model d :: * -> *
  model :: (Backend b) => d -> SMT b (Model d b)

class (DValue p) => Parameter p where
  constraint :: (Backend b) => Model p b -> SMT b (Expr b BoolType)

class (DValue (Store e), Parameter (Param e)) => Effect e where
  type Store e
  type Param e

  allEffects :: [e]

  store :: (Backend b) => e -> SMT b (Model (Store e) b)

  -- * SMT model of the parameter to an effect
  param :: (Backend b)
        => e -> SMT b (Model (Param e) b)

  wp    :: (Backend b)
        => e -> Model (Param e) b -> Pr b (Model (Store e) b)

  eff   :: (Backend b)
        => e -> Model (Param e) b -> Mod b (Model (Store e) b)
