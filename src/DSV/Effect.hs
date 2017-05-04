{-# LANGUAGE DataKinds #-}

module DSV.Effect where

import Language.SMTLib2

import DSV.Logic

class Effect e p where
  allEffects :: [e]
  wp :: (Backend b) => e -> p -> Pr b IntType
  eff :: (Backend b) => e -> p -> Mod b IntType
  arg :: p
