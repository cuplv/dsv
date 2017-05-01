{-# LANGUAGE DataKinds #-}

module DSV.Effect where

import Language.SMTLib2

import DSV.Logic

class Effect e where
  allEffects :: [e]
  wp :: (Backend b) => e -> Pr b IntType
  eff :: (Backend b) => e -> Mod b IntType
