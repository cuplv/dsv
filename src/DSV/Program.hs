{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module DSV.Program
  ( Program (..)
  , Oper
  , Effect
  , accord
  , conflictAvd
  , verifyOp
  , verifyProgram
  , mkOp
  , mkEffect
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Language.SMTLib2
import Control.Monad
import qualified Data.Set as S
import Data.Set (Set)

import DSV.Logic

class (Eq o, Ord o, DataModel (Store o), DataModel (Env o)) => Program o where
  type Store o :: * -> *
  type Env o :: * -> *
  allOps :: [o]
  envConstraint :: (Backend b) => o -> Pr b (Env o b)
  opCon :: (Backend b) => o -> ConReq b (Store o b)
  opDef :: (Backend b) => o -> OperForm b (Env o b) (Store o b)

type Oper b o = Store o b -> Mod b (Store o b)

type Effect b o = Mod b (Store o b)

verifyOp :: (Backend b, Program o) 
         => (Pr b (Store o b), Pr b (Store o b)) 
         -> o 
         -> SMT b (Expr b BoolType)
verifyOp (p,q) o = 
  do env <- model
     snap <- model
     store <- model
     checkCon (p,q) 
              (opCon o) 
              (envConstraint o)
              (opDef o) 
              (env,snap,store)

-- | Predicate stating an "accord" holds for e and g
accord :: (Backend b, Program o)
       => o
       -> Effect b o
       -> ConReq b (Store o b)
       -> SMT b Bool
accord _ e g = 
  (\(Prod (snap,store)) -> g (snap,store)) 
  |= (\(Prod (snap,store)) -> do store' <- e store
                                 g (snap,store'))

verifyProgram :: (Backend b, Program o) => [o] -> Pr b (Store o b) -> SMT b (Expr b BoolType)
verifyProgram os i = mapM (verifyOp (i,i)) os >>= and'

onTwo g = \(Prod (a,b)) -> g (a,b)

-- | Conflict avoidance set for a guard
conflictAvd :: (Backend b, Program o) => [o] -> ConReq b (Store o b) -> SMT b (Set o)
conflictAvd ofs g = 
  do os <- mapM mkEffect ofs
     let gps = [g] ++ map (\o -> wcp o g) os
     let g' = (\store -> and' $ map ($ store) gps)
     result <- onTwo g |= onTwo g'
     if result
        then S.fromList <$> filterM (\o -> not <$> checkAcc o g) ofs
        else conflictAvd ofs g'
  where checkAcc o g = do e <- mkEffect o
                          result <- accord o e g
                          return result

-- | Turn an op form into an op (model the parameters)
mkOp :: (Backend b, Program o) => o -> SMT b (Oper b o)
mkOp o = do env <- model
            assert $ envConstraint o env
            return $ (opDef o) env

-- | Turn an op form into an effect (model parameters /and/ snap)
mkEffect :: (Backend b, Program o) => o -> SMT b (Effect b o)
mkEffect o = mkOp o <*> model
