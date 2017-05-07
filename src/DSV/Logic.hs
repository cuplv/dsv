{-# LANGUAGE DataKinds #-}

module DSV.Logic 
  ( Expr' (..)
  , Pr, Mod
  , triple
  ) where

import Language.SMTLib2

newtype Expr' t b = Expr' { unExpr' :: Expr b t}

type Pr b t = t -> SMT b (Expr b BoolType)

type Mod b t = t -> SMT b t

triple :: (Backend b)
       => (Pr b t, Pr b t) 
       -> Mod b t
       -> t
       -> SMT b (Expr b BoolType)
triple (p,q) op s = p s .=>. (q =<< op s)
