{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}

module DSV 
  ( verify
  , askSMT
  , module DSV.Logic
  , module DSV.Program
  ) where

import Control.Monad
import Language.SMTLib2
import Language.SMTLib2.Pipe
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

askSMT :: SMT SMTPipe a -> IO a
askSMT p = withBackend z3Pipe p

verify' :: (Backend b) => SMTMonad b b -> SMT b (Expr b BoolType) -> SMTMonad b Bool
verify' b p = withBackend b (interp <$> p')
  where p' = not' p >>= assert >> checkSat
        interp Unsat = True
        interp _ = False

