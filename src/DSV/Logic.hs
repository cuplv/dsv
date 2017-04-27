module DSV.Logic where

import Z3.Monad

type Pred = AST -> Z3 AST

-- prePost :: (Pred,Pred) -> Pred -> Z3 AST
-- prePost (p,q) op = 
--   do a <- mkFreshIntVar "a"
--      pre <- p a
--      post <- q =<< op a
--      mkImplies pre post

prePost :: (Pred,Pred) -> Pred -> Z3 AST
prePost (p,q) eff =
  do aSymb <- mkStringSymbol "a"
     intSort <- mkIntSort
     a <- mkConst aSymb intSort
     pre <- p a
     post <- q =<< eff a
     mkForall [] [aSymb] [intSort] =<< mkImplies pre post
