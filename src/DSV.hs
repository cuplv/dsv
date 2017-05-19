{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module DSV 
  ( -- * Sequential logic
    module DSV.Logic
  -- * Verification
  , verify
  , verifyShow
  -- * Propositions
  , program
  , safe
  , seqsafe
  , strong
  , comp
  , consafe
  -- * Contracts
  , module DSV.Contract
  -- * Distributed stores
  , module DSV.Effect
  ) where

import Language.SMTLib2
import Language.SMTLib2.Pipe
import Language.SMTLib2.Debug
import Turtle.Prelude (which)

import DSV.Logic
import DSV.Effect
import DSV.Contract

-- terrible hack, must be fixed!
z3NixPath = "/nix/store/r609yd3wybkjyxrc00ism8j2p29xvw6s-z3-4.5.0/bin/z3"

z3Pipe = do z3 <- which "z3"
            let z3Path = case z3 of
                           Just _ -> "z3"
                           _ -> z3NixPath
            createPipe z3Path ["-smt2","-in"]

-- | Verify a proposition by checking for UNSAT of its negation
verify :: SMT SMTPipe (Expr SMTPipe BoolType) -> IO Bool
verify = verify' z3Pipe

-- | Print out interactions with the SMT solver (in the form of
--   SMTLIB2 expressions) while verifying a proposition
verifyShow 
  :: SMT (DebugBackend SMTPipe) (Expr (DebugBackend SMTPipe) BoolType) 
  -> IO Bool
verifyShow = verify' (debugBackend <$> z3Pipe)

verify' :: (Backend b) => SMTMonad b b -> SMT b (Expr b BoolType) -> SMTMonad b Bool
verify' b p = withBackend b (interp <$> p')
  where p' = not' p >>= assert >> checkSat
        interp Unsat = True
        interp _ = False

-- | Check that a program is safe, given a contract and invariant
-- 
--   This produces an SMT expression representing the soundness of the
--   program with respect to the invariant; if the expression\'s
--   negation is unsatisfiable, then the program is sound
program :: (Backend b, Effect e)
        => [e] -- ^ The program to verify, given as a list of all
               --   forms of the program\'s effects (get this with
               --   @'allEffects' :: 'e'@)
        -> Contract e -- ^ The store contract under which the program
                      -- operates
        -> Pr b (Store e b) -- ^ The store invariant that the program
                            -- must respect
        -> SMT b (Expr b BoolType)
program es c i = and' (map (consafe c i) es)

-- | Check that a particular effect always maintains an invariant
safe :: (Backend b, Effect e)
     => Pr b (Store e b) -- ^ The invariant
     -> e -- ^ The effect
     -> SMT b (Expr b BoolType)
safe i e = do n <- param e
              let i' = \a -> i a .&. (constraint n)
              a <- model
              triple (i',i') (eff e n) a

-- | Check that an effect always maintains an invariant on stores that
--   satisfy its weakest precondition
-- 
--   This essentially means that the effect is \"safe\" as long as it
--   is operating on a non-distributed (or strongly-consistent) store.
seqsafe :: (Backend b, Effect e)
        => Pr b (Store e b)
        -> e 
        -> SMT b (Expr b BoolType)
seqsafe i e = do n <- param e
                 let i' = \a -> i a .&. (constraint n)
                     pre = \a -> i' a .&. wp e n a
                 a <- model
                 triple (pre,i) (eff e n) a

-- | Check that a contract requires that two effects see each other
-- 
--   (This just lifts the result of 'vis' into an SMT expression)
strong :: (Backend b, Effect e) 
       => Contract e 
       -> (e,e) 
       -> SMT b (Expr b BoolType)
strong c (e0,e1) = if vis c (e0,e1)
                      then true
                      else false

-- | Check that two effects cannot together violate an invariant
comp :: (Backend b, Effect e)
     => Contract e 
     -> Pr b (Store e b)
     -> (e,e) 
     -> SMT b (Expr b BoolType)
comp c i (e0,e1) = do n <- param e1
                      strong c (e0,e1) .|. safe (wp e1 n) e0

-- | Check that an effect always maintains an invariant under a
--   particular contract
consafe :: (Backend b, Effect e)
        => Contract e 
        -> Pr b (Store e b)
        -> e 
        -> SMT b (Expr b BoolType)
consafe c i e = seqsafe i e .&. (and' (map (\e0 -> comp c i (e0,e)) allEffects))
