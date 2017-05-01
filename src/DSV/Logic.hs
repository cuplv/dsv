{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module DSV.Logic where

import Turtle.Prelude (which)

import Z3.Monad hiding (assert)
import Language.SMTLib2
import Language.SMTLib2.Pipe
import Language.SMTLib2.Debug

type Pred = AST -> Z3 AST

prePost :: (Pred,Pred) -> Pred -> Z3 AST
prePost (p,q) op = 
  do a <- mkFreshIntVar "a"
     pre <- p a
     post <- q =<< op a
     mkImplies pre post

type Pr b = Expr b IntType -> Expr b BoolType

type Mod b = Expr b IntType -> Expr b IntType

triple :: (Backend b) => (Pr b, Pr b) -> Mod b -> SMT b (Expr b BoolType)
triple (p,q) op = declareVar int >>= (\a -> p a .=>. q (op a))


test :: Backend b => SMT b (Integer,Integer)
test = do
  x <- declareVar int
  y <- declareVar int
  assert $ x .+. y .==. cint 5
  checkSat
  IntValue vx <- getValue x
  IntValue vy <- getValue y
  return (vx,vy)

z3Path = "/nix/store/r609yd3wybkjyxrc00ism8j2p29xvw6s-z3-4.5.0/bin/z3"

runTest = withBackend (createPipe z3Path ["-smt2","-in"]) test >>= print 

runTest' = withBackend (debugBackend <$> (createPipe z3Path ["-smt2","-in"])) test >>= print
