{-# LANGUAGE DataKinds #-}

module DSV.Logic 
  ( Pr, Mod
  , triple
  , ConReq
  , Oper
  , presVis
  , presAny
  , checkCon
  , wcp
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


-- | A "consistency requirement" on store type 't' (also called a
--   "consistency guard", "consistency predicate", or "consistency
--   refinement"
type ConReq b t = Pr b (t,t)

-- | Transformer on store given environment of type 'e', first bound
--   with a snap
type Oper b e t = e -> t -> t -> SMT b t

presVis :: (Backend b)
        => (ConReq b t, ConReq b t) -- ^ Pre- and post- consistency
                                    --   conditions
        -> Pr b e -- ^ Environment (arg) constraints
        -> Oper b e t
        -> (e,t,t,t) -- ^ (Env, Snap, Global, Local) state model
        -> SMT b (Expr b BoolType)
presVis (k1,k2) ep c = triple (k1',k2') c'
  where c' (env,sn,gl,lc) = 
          do gl' <- c env sn gl
             lc' <- c env sn lc
             return (env,sn,gl',lc')

        k1' (env,sn,gl,lc) = k1 (lc,gl) .&. k1 (sn,gl) .&. ep env
        k2' (env,sn,gl,lc) = k2 (lc,gl)

presAny :: (Backend b)
        => (ConReq b t, ConReq b t)
        -> Pr b e -- ^ environment constraints
        -> Oper b e t
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
         -> Oper b e t
         -> (e,t,t) -- ^ (env,snap,store)
         -> SMT b (Expr b BoolType)
checkCon (p,q) k ep c (env,snap,store) = 
  let p' = \t -> p t .&. k (snap,t) .&. ep env
  in triple (p',q) (c env snap) store

wcp :: (Backend b) => (t -> SMT b t) -> ConReq b t -> ConReq b t
wcp c k = \(snap,store) -> do snap' <- c snap
                              store' <- c store
                              k (snap',store')
