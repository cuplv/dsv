{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}

module DSV.Prelude where

import Language.SMTLib2

import DSV

-- | An empty value containing no information
data UnitM b = UnitM

instance DataModel UnitM where
  model = return UnitM
  modEq _ _ = true

-- | A boolean model type, represented as a single 'BoolType' SMT
--   expression
data BoolM b = BoolM (Expr b BoolType)

instance DataModel BoolM where
  model = BoolM <$> declareVar bool
  modEq (BoolM b1) (BoolM b2) = b1 .==. b2

-- | An integer model type, represented as a single 'IntType' SMT
--   expression
data IntM b = IntM (Expr b IntType)

instance DataModel IntM where
  model = IntM <$> declareVar int
  modEq (IntM i1) (IntM i2) = i1 .==. i2

top :: (Backend b) => Pr b (t b)
top _ = true

-- | A predicate stating that an 'IntM' be non-negative
nonNegative :: (Backend b) => Pr b (IntM b)
nonNegative (IntM a) = a .>=. cint 0

-- | a predicate stating that an 'IntM' be greater than zero
positive :: (Backend b) => Pr b (IntM b)
positive (IntM a) = a .>=. cint 1

conTop :: (Backend b) => ConReq b (t b)
conTop _ = true

conEq :: (Backend b, DataModel t) => ConReq b (t b)
conEq (a,b) = modEq a b

conBot :: (Backend b) => ConReq b (t b)
conBot _ = false

conLE :: (Backend b) => ConReq b (IntM b)
conLE ((IntM snap),(IntM store)) = snap .<=. store

conGE :: (Backend b) => ConReq b (IntM b)
conGE ((IntM snap),(IntM store)) = snap .>=. store


-- BANK ACCOUNTS

data Bank = Withdraw | Deposit deriving (Show,Eq,Ord)

instance Program Bank where
  type Store Bank = IntM
  type Env Bank = IntM
  
  allOps = [Withdraw,Deposit]
  envConstraint _ = positive

  opCon Withdraw = conLE
  opCon Deposit = conTop

  opDef Withdraw (IntM n) (IntM snap) (IntM store) = 
    IntM <$> ite (snap .>=. n) (store .-. n) (store)
  opDef Deposit (IntM n) _ (IntM store) = 
    IntM <$> store .+. n

data BankR = BankR Bank | Reset deriving (Show,Eq,Ord)

instance Program BankR where
  type Store BankR = IntM
  type Env BankR = IntM

  allOps = map BankR (allOps) ++ [Reset]
  envConstraint _ = positive

  opCon (BankR Withdraw) = conLE
  opCon (BankR Deposit) = conTop
  opCon Reset = conTop

  opDef (BankR b) = opDef b
  opDef Reset = \_ _ _ -> IntM <$> cint 0


-- CONSPIRING BOOLEANS

data B3 b = B3 (Expr b BoolType) (Expr b BoolType) (Expr b BoolType)

cfst :: (Backend b) => ConReq b (B3 b)
cfst ((B3 a _ _),(B3 d _ _)) = a .==. d

csnd :: (Backend b) => ConReq b (B3 b)
csnd ((B3 _ b _),(B3 _ e _)) = b .==. e

true2or3 :: (Backend b) => Pr b (B3 b)
true2or3 (B3 _ b c) = b .|. c

instance DataModel B3 where
  model = B3 <$> declareVar bool 
             <*> declareVar bool 
             <*> declareVar bool
  modEq (B3 a b c) (B3 d e f) = 
    (a .==. d) .&. (b .==. e) .&. (c .==. f)


data ConspireBools = E1 | E2 | E3 deriving (Show,Eq,Ord)

instance Program ConspireBools where
  type Store ConspireBools = B3
  type Env ConspireBools = UnitM

  allOps = [E1,E2,E3]
  envConstraint _ = top

  opCon E3 = csnd
  opCon _ = conTop

  opDef E1 _ _ (B3 _ e f) = B3 <$> true <*> pure e <*> pure f
  opDef E2 _ (B3 a _ _) (B3 d e f) = 
    B3 <$> pure d <*> ite a e false <*> pure f
  opDef E3 _ (B3 a b c) (B3 d e f) = 
    B3 <$> pure d <*> pure e <*> ite b false f
