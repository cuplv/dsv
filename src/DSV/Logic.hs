{-# LANGUAGE DataKinds #-}

module DSV.Logic 
  ( Pr, Mod
  , triple
  ) where

import Language.SMTLib2

-- | Reverse argument order on 'Expr', so that backend 'b' can be left
--   unapplied
newtype Expr' t b = Expr' { unExpr' :: Expr b t}

-- | Logical predicate containing one free variable of type 't'
--   (modeled as a function from 't' to an SMT expression of type
--   'BoolType')
type Pr b t = t -> SMT b (Expr b BoolType)

-- | Transformer on an SMT expression of type 't'
type Mod b t = t -> SMT b t

-- | Construct an SMT expression verifying a Hoare triple. 
--
--   A Hoare triple is a logical statement of the form @{P} C {Q}@,
--   where @P@ is the pre-condition of a program\'s state, @Q@ is the
--   post-condition, and @C@ is the program command that should take
--   the state from @P@ to @Q@.
triple :: (Backend b)
       => (Pr b t, Pr b t) -- ^ Pre- and post-condition (P and Q)
       -> Mod b t -- ^ Transformer command (C)
       -> t -- ^ State model
       -> SMT b (Expr b BoolType)
triple (p,q) op s = p s .=>. (q =<< op s)
