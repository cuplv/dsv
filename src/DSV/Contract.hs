module DSV.Contract
  ( Contract (..)
  , vis
  , emptyC
  , strongC
  ) where

-- | A constraint on effect soups which determines which effects must
--   be visible to one another, based on the forms of those effects
data Contract e = Vis ((e,e) -> Bool)

-- | From a 'Contract', determine whether two effects must be visible
--   to one another
vis :: Contract e -> (e,e) -> Bool
vis (Vis f) = f

-- | The empty contract, which requires no visibility
emptyC :: Contract e
emptyC = Vis (const False)

-- | The total contract, which requires strong consistency for all
--   effects
strongC :: Contract e
strongC = Vis (const True)
