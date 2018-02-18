{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
-- {-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE UndecidableInstances #-}

module DSV.Prelude where

import Language.SMTLib2
-- import Language.Haskell.TH

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

-- | An array of integers
data ArrayIntM b = ArrayIntM (Expr b (ArrayType '[IntType] IntType))

instance DataModel ArrayIntM where
  model = ArrayIntM <$> declareVar (array (int ::: Nil) int)
  modEq (ArrayIntM a) (ArrayIntM b) = a .==. b

onArray :: (Backend b) => ConReq b (IntM b) -> ConReq b (ArrayIntM b)
onArray con (ArrayIntM sn,ArrayIntM st) = 
  do i <- declareVar int
     assert $ (i .>=. cint 0) .&. (i .<. cint 10)
     i <- cint 0
     snI <- IntM <$> select1 sn i
     stI <- IntM <$> select1 st i
     con (snI,stI)

conAllLE :: (Backend b) => ConReq b (ArrayIntM b)
conAllLE (ArrayIntM sn, ArrayIntM st) =
  sumUp sn [0..10] .<=. sumUp st [0..10]
  where sumUp arr is = 
          mapM (\i -> select1 arr (cint i)) is >>= plus

conAllGE :: (Backend b) => ConReq b (ArrayIntM b)
conAllGE (ArrayIntM sn, ArrayIntM st) =
  sumUp sn [0..10] .>=. sumUp st [0..10]
  where sumUp arr is = 
          mapM (\i -> select1 arr (cint i)) is >>= plus

conAllEq :: (Backend b) => ConReq b (ArrayIntM b)
conAllEq (ArrayIntM sn, ArrayIntM st) =
  sumUp sn [0..10] .==. sumUp st [0..10]
  where sumUp arr is = 
          mapM (\i -> select1 arr (cint i)) is >>= plus

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

onJBA :: (Backend b) => ConReq b (IntM b) -> ConReq b (JBA b)
onJBA con (snap,store) = let (snapA,_,_) = deconJBA snap
                             (storeA,_,_) = deconJBA store
                         in con (snapA,storeA)

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

  opCon _ = conTop

  opDef E1 _ _          (B3 _ e f) = 
    B3 <$> true <*> pure e <*> pure f
  opDef E2 _ _          (B3 d e f) = 
    B3 <$> pure d <*> ite d e false <*> pure f
  opDef E3 _ _          bs         = 
    pure bs

data ABC = A | B | C deriving (Show,Eq,Ord)

instance Program ABC where
  type Store ABC = IntM
  type Env ABC = UnitM
  
  allOps = [A,B,C]
  envConstraint _ = top
  
  opCon _ = conTop
  
  opDef A _ _ (IntM s) = 
    IntM <$> ite (s .==. cint 0 .|. s .==. cint 1) (s .+. cint 1) (s)
  opDef B _ _ (IntM s) = 
    IntM <$> ite (s .==. cint 2) (cint 0) (s)
  opDef C _ _ s = pure s

-- | Snap and store agree on whether the state is 1
con1 :: (Backend b) => ConReq b (Store ABC b)
con1 (IntM snap,IntM store) = (snap .==. cint 1) .==. (store .==. cint 1)
     

data KVBank = KVBank Bank deriving (Show,Eq,Ord)

instance Program KVBank where
  type Store KVBank = ArrayIntM
  type Env KVBank = Prod IntM (Env Bank)
  
  allOps = map KVBank allOps
  envConstraint _ (Prod (IntM i,n)) = 
    and' [i .>=. cint 0
         ,i .<. cint 10
         ,envConstraint Withdraw n]
  
  opCon _ = conTop

  opDef (KVBank o) (Prod (IntM i,n)) (ArrayIntM sn) (ArrayIntM st) = 
    do snI <- IntM <$> select1 sn i
       stI <- IntM <$> select1 st i
       IntM stI' <- opDef o n snI stI
       ArrayIntM <$> store1 st i stI'

-- | A bank account requiring requests and approvals for withdrawals
data JointBank = R1 -- ^ Request withdrawal for owner #1
               | R2 -- ^ Request withdrawal for owner #2
               | A1 -- ^ Approve withdrawal for owner #1
               | A2 -- ^ Approve withdrawal for owner #2
               | Dp -- ^ Deposit (owner \#1 or \#2)
               | Wd1 -- ^ Withdraw (by owner #1)
               | Wd2 -- ^ Withdraw (by owner #2)
               deriving (Show,Eq,Ord)

type JBA = Prod IntM (Prod (Prod BoolM BoolM) (Prod BoolM BoolM))

deconJBA :: JBA b -> (IntM b,(BoolM b,BoolM b),(BoolM b,BoolM b))
deconJBA (Prod (a, Prod (Prod rs, Prod as))) = (a,rs,as)

mkJBA :: (IntM b,(BoolM b,BoolM b),(BoolM b,BoolM b)) -> JBA b
mkJBA (a,rs,as) = Prod (a, Prod (Prod rs, Prod as))

conReadiness :: (Backend b) => JointBank -> ConReq b (JBA b)
conReadiness o (snap,store) = 
  let (_,_,(BoolM appA1,BoolM appA2)) = deconJBA snap
      (_,_,(BoolM appB1,BoolM appB2)) = deconJBA store
  in case o of
       Wd1 -> appA1 .==. appB1
       Wd2 -> appA2 .==. appB2

instance Program JointBank where
  type Store JointBank = JBA
  type Env JointBank = IntM
  allOps = [R1,R2,A1,A2,Dp,Wd1,Wd2]
  envConstraint _ = positive

  -- For now we're not worrying about invariants
  opCon _ = conTop
  
  opDef R1 _ _ s = 
    let (a,(_,r2),(a1,a2)) = deconJBA s 
    in do r1 <- BoolM <$> true
          return $ mkJBA (a,(r1,r2),(a1,a2))
  opDef R2 _ _ s = 
    let (a,(r1,_),(a1,a2)) = deconJBA s 
    in do r2 <- BoolM <$> true
          return $ mkJBA (a,(r1,r2),(a1,a2))
  opDef A1 _ _ s = 
    let (a,(BoolM req,r2),(_,a2)) = deconJBA s 
    in do a1 <- BoolM <$> pure req
          r1 <- BoolM <$> not' req
          return $ mkJBA (a,(r1,r2),(a1,a2))
  opDef A2 _ _ s = 
    let (a,(r1,BoolM req),(a1,_)) = deconJBA s 
    in do a2 <- BoolM <$> pure req
          r2 <- BoolM <$> not' req
          return $ mkJBA (a,(r1,r2),(a1,a2))
  opDef Dp (IntM n) _ s = 
    let (IntM a,rs,as) = deconJBA s
    in do a' <- IntM <$> a .+. n
          return $ mkJBA (a',rs,as)
  opDef Wd1 (IntM n) snapS storeS = 
    let (IntM store,(r1,r2),(_,a2)) = deconJBA storeS
        (IntM snap ,_,(BoolM app,_)) = deconJBA snapS
    in do cond <- (snap .>=. n) .&. app
          store' <- IntM <$> ite cond (store .-. n) store
          -- r1 <- BoolM <$> ite cond false req
          a1 <- BoolM <$> ite cond false app
          return $ mkJBA (store',(r1,r2),(a1,a2))
  opDef Wd2 (IntM n) snapS storeS = 
    let (IntM store,(r1,r2),(a1,_)) = deconJBA storeS
        (IntM snap ,_,(_,BoolM app)) = deconJBA snapS
    in do cond <- (snap .>=. n) .&. app
          store' <- IntM <$> ite cond (store .-. n) store
          -- r2 <- BoolM <$> ite cond false req
          a2 <- BoolM <$> ite cond false app
          return $ mkJBA (store',(r1,r2),(a1,a2))
