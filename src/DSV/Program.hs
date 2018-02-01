{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module DSV.Program
  ( Program (..)
  , verifyOp
  , verifyProgram
  ) where

import Data.Map (Map)
import qualified Data.Map as M
import Language.SMTLib2

import DSV.Model
import DSV.Logic

class (DataModel (Store o), DataModel (Env o)) => Program o where
  type Store o :: * -> *
  type Env o :: * -> *
  allOps :: [o]
  envConstraint :: (Backend b) => o -> Pr b (Env o b)
  opCon :: (Backend b) => o -> ConReq b (Store o b)
  opDef :: (Backend b) => o -> Oper b (Env o b) (Store o b)

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


verifyProgram :: (Backend b, Program o) => [o] -> Pr b (Store o b) -> SMT b (Expr b BoolType)
verifyProgram os i = mapM (verifyOp (i,i)) os >>= and'
