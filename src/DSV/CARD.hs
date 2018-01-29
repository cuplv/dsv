{-# LANGUAGE DataKinds #-}

module DSV.CARD where

import Language.SMTLib2
import DSV.Logic
import DSV.Effect

import DSV.Prelude

-- | A "consistency requirement" on store type 't' (also called a
--   "consistency guard", "consistency predicate", or "consistency
--   refinement"
type ConReq b t = Pr b (t,t)

-- | Transformer on store, first bound with a snap
type Oper b t = t -> t -> SMT b t

presVis :: (Backend b)
        => (ConReq b t, ConReq b t) -- ^ Pre- and post- consistency
                                    --   conditions
        -> Oper b t
        -> (t,t,t) -- ^ (Snap, Global, Local) three-part state model
        -> SMT b (Expr b BoolType)
presVis (k1,k2) c = triple (k1',k2') c'
  where c' (sn,gl,lc) = do gl' <- c sn gl
                           lc' <- c sn lc
                           return (sn,gl',lc')
        k1' (sn,gl,lc) = k1 (lc,gl) .&. k2 (sn,gl)
        k2' (sn,gl,lc) = k2 (lc,gl)

presAny :: (Backend b)
        => (ConReq b t, ConReq b t)
        -> Oper b t
        -> (t,t)
        -> SMT b (Expr b BoolType)
presAny ks c = triple ks (\(sg,sr) -> (,) <$> c sr sg <*> pure sr)

checkCon :: (Backend b)
         => (Pr b t, Pr b t)
         -> ConReq b t
         -> Oper b t
         -> (t,t) -- ^ (snap,store)
         -> SMT b (Expr b BoolType)
checkCon (p,q) k c (snap,store) = 
  let p' = \t -> p t .&. k (snap,t)
  in triple (p',q) (c snap) store


conLE :: (Backend b) => ConReq b (IntM b)
conLE ((IntM snap),(IntM store)) = snap .<=. store

withdraw1 :: (Backend b) => Oper b (IntM b)
withdraw1 (IntM snap) (IntM store) = 
  let n = cint 1
  in IntM <$> ite (snap .>=. n) (store .-. n) (store)


withdrawTest :: (Backend b) => SMT b (Expr b BoolType)
withdrawTest = do (snap,store) <- (,) <$> model <*> model
                  checkCon (nonNegative,nonNegative)
                           conLE
                           withdraw1
                           (snap,store)
