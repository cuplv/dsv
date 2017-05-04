{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilyDependencies #-}

module DSV.Effect where

import Language.SMTLib2

import DSV.Logic

class Effect e where
  type Arg e :: * -> * -- = (r :: * -> *) | r -> e
  type ArgM e :: * -> * -> *
  allEffects :: [e]
  wp :: (Backend b) => e -> Arg e b -> Pr b IntType
  eff :: (Backend b) => e -> Arg e b -> Mod b IntType
  arg :: (Backend b) => e -> ArgM e b (Arg e b)
  argc :: (Backend b) => e -> Arg e b -> ArgM e b (Expr b BoolType)

-- invar :: (Backend b)
