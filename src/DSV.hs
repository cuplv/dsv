{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE GADTs #-}

module DSV where

import Language.SMTLib2
import Language.SMTLib2.Pipe
import Language.SMTLib2.Debug
import Turtle.Prelude (which)

import DSV.Logic
import DSV.Effect
import DSV.Contract
import DSV.Effect.Bank

z3NixPath = "/nix/store/r609yd3wybkjyxrc00ism8j2p29xvw6s-z3-4.5.0/bin/z3"

z3Pipe = do z3 <- which "z3"
            let z3Path = case z3 of
                           Just _ -> "z3"
                           _ -> z3NixPath
            createPipe z3Path ["-smt2","-in"]

-- * Verify a proposition by checking for UNSAT of its negation
verify = verify' z3Pipe
debug = verify' (debugBackend <$> z3Pipe)

verify' :: (Backend b) => SMTMonad b b -> SMT b (Expr b BoolType) -> SMTMonad b Bool
verify' b p = withBackend b (interp <$> p')
  where p' = not' p >>= assert >> checkSat
        interp Unsat = True
        interp _ = False

-- * Check that a program is safe, given a contract and invariant
program :: (Backend b, Effect e, ArgM e b ~ SMT b) 
        => [e] 
        -> Contract e 
        -> Pr b IntType 
        -> SMT b (Expr b BoolType)
program es c i = and' (map (consafe c i) es)

safe :: (Backend b, Effect e, ArgM e b ~ SMT b)
     => Pr b IntType
     -> e 
     -> SMT b (Expr b BoolType)
safe i e = do n <- arg e
              let i' = \a -> i a .&. (argc e n)
              triple (i',i') (eff e n)

seqsafe :: (Backend b, Effect e, ArgM e b ~ SMT b) 
        => Pr b IntType
        -> e 
        -> SMT b (Expr b BoolType)
seqsafe i e = do n <- arg e
                 let i' = \a -> i a .&. (argc e n)
                     pre = \a -> i' a .&. wp e n a
                 triple (pre,i) (eff e n)

strong :: (Backend b, Effect e) 
       => Contract e 
       -> (e,e) 
       -> SMT b (Expr b BoolType)
strong c (e0,e1) = if vis c (e0,e1)
                      then true
                      else false

comp :: (Backend b, Effect e, ArgM e b ~ SMT b) 
     => Contract e 
     -> Pr b IntType
     -> (e,e) 
     -> SMT b (Expr b BoolType)
comp c i (e0,e1) = do n <- arg e1
                      strong c (e0,e1) .|. safe (wp e1 n) e0

consafe :: (Backend b, Effect e, ArgM e b ~ SMT b) 
        => Contract e 
        -> Pr b IntType
        -> e 
        -> SMT b (Expr b BoolType)
consafe c i e = seqsafe i e .&. (and' (map (\e0 -> comp c i (e0,e)) allEffects))
