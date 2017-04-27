module DSV.Contract where

data Contract e = Vis ((e,e) -> Bool)

vis :: Contract e -> (e,e) -> Bool
vis (Vis f) = f

emptyC = Vis (const False)

strongC = Vis (const True)
