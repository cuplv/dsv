module DSV.Logic where

import Z3.Monad

type Pred = AST -> Z3 AST

prePost :: (Pred,Pred) -> Pred -> Z3 AST
prePost (p,q) op = 
  do a <- mkFreshIntVar "a"
     pre <- p a
     post <- q =<< op a
     mkImplies pre post
