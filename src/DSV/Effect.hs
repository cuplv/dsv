module DSV.Effect where

import DSV.Logic

class Effect e where
  allEffects :: [e]
  wp :: e -> Pred
  eff :: e -> Pred
