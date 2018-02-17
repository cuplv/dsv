{-# LANGUAGE DataKinds #-}

module DSV.Logic 
  ( DataModel(..), Pr, Mod, Prod(..)
  , triple
  , ConReq
  , OperForm
  , impl
  , entails, (|=), taut
  , presVis
  , presAny
  , checkCon
  , wcp
  ) where


import Language.SMTLib2

-- | A datatype which can be universally quantified as a structure of
--   SMT variables
class DataModel d where

  -- | Produce SMT variables representing the universal quantification
  --   of this datatype
  model :: (Backend b) => SMT b (d b)
  -- | Produce an expression stating that two models are equal
  modEq :: (Backend b) => d b -> d b -> SMT b (Expr b BoolType)

data Prod a b x = Prod { unProd :: (a x, b x) }

instance (DataModel a, DataModel b) => DataModel (Prod a b) where
  model = do a <- model
             b <- model
             return $ Prod (a,b)
  modEq (Prod (a,b)) (Prod (c,d)) = (modEq a c) .&. (modEq b d)

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

-- | A "consistency requirement" on store type 't' (also called a
--   "consistency guard", "consistency predicate", or "consistency
--   refinement"
type ConReq b t = Pr b (t,t)

impl :: (Backend b, DataModel t) => ConReq b (t b) -> ConReq b (t b) -> SMT b (Expr b BoolType)
impl k k' = do snap <- model
               store <- model
               k (snap,store) .=>. k' (snap,store)

-- | Check if for all models where the first predicate is true, the
--   second is also true.  If this cannot be decided, it is considered false.
entails :: (Backend b, DataModel t) => Pr b (t b) -> Pr b (t b) -> SMT b Bool
entails p1 p2 = do push
                   m <- model
                   assert $ not' (p1 m .=>. p2 m)
                   result <- checkSat
                   pop
                   return (case result of
                             Unsat -> True
                             _ -> False)

-- | Fancy operator syntax for 'entails'
(|=) :: (Backend b, DataModel t) => Pr b (t b) -> Pr b (t b) -> SMT b Bool
(|=) a b = entails a b

-- | Check that predicate is a tautology.  "@taut p@" is equivalent to
--   "@pure true |= p@"
taut :: (Backend b, DataModel t) => Pr b (t b) -> SMT b Bool
taut b = pure true |= b

-- | Transformer on store given environment of type 'e', first bound
--   with a snap
type OperForm b e t = e -> t -> t -> SMT b t

presVis :: (Backend b)
        => (ConReq b t, ConReq b t) -- ^ Pre- and post- consistency
                                    --   conditions
        -> Pr b e -- ^ Environment (arg) constraints
        -> OperForm b e t
        -> (e,t,t,t) -- ^ (Env, Snap, Global, Local) state model
        -> SMT b (Expr b BoolType)
presVis (k1,k2) ep c = triple (k1',k2') c'
  where c' (env,sn,gl,lc) = 
          do gl' <- c env sn gl
             lc' <- c env sn lc
             return (env,sn,gl',lc')

        k1' (env,sn,gl,lc) = k1 (lc,gl) .&. k1 (sn,gl) .&. ep env
        k2' (env,sn,gl,lc) = k2 (lc,gl)

-- | SMT decision for "accords"
presAny :: (Backend b)
        => (ConReq b t, ConReq b t)
        -> Pr b e -- ^ environment constraints
        -> OperForm b e t
        -> (e,t,t,t) -- ^ (Env, Snap, Global, Local)
        -> SMT b (Expr b BoolType)
presAny (k1,k2) ep c s = triple (k1',k2') c' s .&. triple (k1',k2') c'' s
  where c' (env,sn,gl,lc) = 
          do gl' <- c env sn gl
             lc' <- c env sn lc
             return (env,sn,gl',lc')

        c'' (env,sn,gl,lc) = 
          do gl' <- c env sn gl
             return (env,sn,gl',lc) -- lc does not change for this
                                    -- test

        k1' (env,sn,gl,lc) = k1 (lc,gl) .&. k1 (sn,gl) .&. ep env
        k2' (env,sn,gl,lc) = k2 (lc,gl)

checkCon :: (Backend b)
         => (Pr b t, Pr b t)
         -> ConReq b t
         -> Pr b e -- ^ environment constraints
         -> OperForm b e t
         -> (e,t,t) -- ^ (env,snap,store)
         -> SMT b (Expr b BoolType)
checkCon (p,q) k ep c (env,snap,store) = 
  let p' = \t -> p t .&. k (snap,t) .&. ep env
  in triple (p',q) (c env snap) store

wcp :: (Backend b) => (t -> SMT b t) -> ConReq b t -> ConReq b t
wcp c k = \(snap,store) -> do snap' <- c snap
                              store' <- c store
                              k (snap',store')
