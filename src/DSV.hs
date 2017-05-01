{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module DSV where

import Language.SMTLib2
import Language.SMTLib2.Pipe
import Language.SMTLib2.Debug

import DSV.Logic
import DSV.Effect
import DSV.Contract
import DSV.Effect.Bank

z3Path = "/nix/store/r609yd3wybkjyxrc00ism8j2p29xvw6s-z3-4.5.0/bin/z3"

z3Pipe = createPipe z3Path ["-smt2","-in"]

-- * Verify a proposition by checking for UNSAT of its negation
verify = verify' z3Pipe
debug = verify' (debugBackend <$> z3Pipe)

verify' :: (Backend b) => SMTMonad b b -> SMT b (Expr b BoolType) -> SMTMonad b Bool
verify' b p = withBackend b (interp <$> p')
  where p' = not' p >>= assert >> checkSat
        interp Unsat = True
        interp _ = False

-- * Check that a program is safe, given a contract and invariant
program :: (Backend b, Effect e) 
        => [e] 
        -> Contract e 
        -> Pr b IntType 
        -> SMT b (Expr b BoolType)
program es c i = and' (map (consafe c i) es)

safe :: (Backend b, Effect e) 
     => Pr b IntType
     -> e 
     -> SMT b (Expr b BoolType)
safe i e = triple (i,i) (eff e)

seqsafe :: (Backend b, Effect e) 
        => Pr b IntType
        -> e 
        -> SMT b (Expr b BoolType)
seqsafe i e = let pre a = i a .&. wp e a
              in triple (pre,i) (eff e)

strong :: (Backend b, Effect e) 
       => Contract e 
       -> (e,e) 
       -> SMT b (Expr b BoolType)
strong c (e0,e1) = if vis c (e0,e1)
                      then true
                      else false

comp :: (Backend b, Effect e) 
     => Contract e 
     -> Pr b IntType
     -> (e,e) 
     -> SMT b (Expr b BoolType)
comp c i (e0,e1) = strong c (e0,e1) .|. safe (wp e1) e0

consafe :: (Backend b, Effect e) 
        => Contract e 
        -> Pr b IntType
        -> e 
        -> SMT b (Expr b BoolType)
consafe c i e = seqsafe i e .&. (and' (map (\e0 -> comp c i (e0,e)) allEffects))
