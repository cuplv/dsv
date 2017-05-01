{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}

module DSV.Logic where

import Language.SMTLib2

type Pr b t = Expr b t -> SMT b (Expr b BoolType)

type Mod b t = Expr b t -> SMT b (Expr b t)

triple :: (Backend b)
       => (Pr b IntType, Pr b IntType) 
       -> Mod b IntType
       -> SMT b (Expr b BoolType)
triple (p,q) op = declareVar int >>= (\a -> p a .=>. (q =<< op a))
