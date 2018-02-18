{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module DSV 
  ( verify
  , verifyShow
  , askSMT
  , module DSV.Logic
  , module DSV.Program
  ) where

import Control.Monad
import Language.SMTLib2
import Language.SMTLib2.Pipe
import Language.SMTLib2.Debug
import Turtle.Prelude (which)

import DSV.Logic
import DSV.Program

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

askSMT :: SMT (DebugBackend SMTPipe) a -> IO a
askSMT p = withBackend (debugBackend <$> z3Pipe) p

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

