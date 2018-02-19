module Main where

import System.Exit
import Data.Maybe
import Language.SMTLib2
import Language.SMTLib2.Pipe
import Language.SMTLib2.Debug
import Data.List (sort)
import qualified Data.Set as S
import Data.Set (Set)

import DSV
import DSV.Prelude


tests :: [IO (Maybe String)]
tests = kvBank


allTests = concat [sanity,bank,bankR,conspireBools,jointBank,kvBank,stateMachine]

sanity =
  [ver (pure true |= \(IntM a) -> a .==. cint 2) False "Sanity 1"
  ,ver ((\(IntM a) -> a .==. cint 2) |= (\(IntM a) -> a .==. cint 2)) True "Sanity 2"]

bank = 
  [confl [Withdraw] conLE "LE"
  ,confl [Deposit] conGE "GE"

  ,confl [Withdraw,Deposit] conEq "EQ"
  ,confl ([] :: [Bank]) conTop "Top - Bank"]
bankR = 
  [confl [BankR Withdraw,Reset] conLE "LE - R"
  ,confl [BankR Deposit,Reset] conGE "GE - R"

  ,confl [BankR Withdraw,BankR Deposit,Reset] conEq "EQ - BankR"
  ,confl ([] :: [BankR]) conTop "Top - BankR"]
conspireBools = 
  [confl [E1] cfst "First"
  ,confl [E1,E2] csnd "Second"

  ,confl [E1,E2] conEq "Eq"
  ,confl ([] :: [ConspireBools]) conTop "Top - Conspire"]
jointBank = 
  [confl [Wd1,Wd2] (onJBA conLE) "LE - JBA"
  ,confl [Dp] (onJBA conGE) "GE - JBA"
  ,confl [R1,A1,Wd1] (conReadiness Wd1) "Ready 1?"
  ,confl [R2,A2,Wd2] (conReadiness Wd2) "Ready 2?"

  ,confl [R1,R2,A1,A2,Wd1,Wd2,Dp] conEq "EQ - JointBank"
  ,confl ([] :: [JointBank]) conTop "Top - JointBank"]
kvBank = 
  [confl [KVBank Withdraw,On1 Withdraw,On2 Withdraw] (onArray conLE) "LE - KV"
  ,confl [KVBank Deposit,On1 Deposit,On2 Deposit] (onArray conGE) "GE - KV"
  ,confl [KVBank Withdraw, KVBank Deposit,On1 Withdraw,On1 Deposit,On2 Withdraw,On2 Deposit] (onArray conEq) "EQ - KV"
  ,confl [KVBank Withdraw,On1 Withdraw,On2 Withdraw] conAllLE "LE - KV all 10"
  ,confl [KVBank Deposit,On1 Deposit,On2 Deposit] conAllGE "GE - KV all 10"
  ,confl [KVBank Withdraw, KVBank Deposit,On1 Withdraw,On1 Deposit,On2 Withdraw,On2 Deposit] conAllEq "Eq - KV all 10"

  ,confl [KVBank Withdraw,On1 Withdraw] (onArray1 conLE) "LE - KV 1"
  ,confl [KVBank Deposit,On1 Deposit] (onArray1 conGE) "GE - KV 1"

  ,confl [KVBank Withdraw, KVBank Deposit ,On1 Withdraw,On1 Deposit,On2 Withdraw,On2 Deposit] conEq "Eq - KV total"
  ,confl ([] :: [KVBank]) conTop "Top - KVBank"]
stateMachine = 
  [confl ([A,B]::[ABC]) con1 "1"

  ,confl [A,B] conEq "Eq - ABC"
  ,confl ([] :: [ABC]) conTop "Top - ABC"]

-- | A test of the conflict avoidance set for a guard
confl :: (Program o, Eq o, Show o)
      => [o] -- ^ Expected conflicting operations
      -> ConReq SMTPipe (Store o SMTPipe) -- ^ Guard to test
      -> String -- ^ Helpful name for this test
      -> IO (Maybe String)
confl os g = ver (conflictAvd allOps g) (S.fromList os)

-- | A test running an experiment on an SMT solver.
ver :: (Eq a, Show a)
   => SMT SMTPipe a -- ^ Question for SMT solver
   -> a -- ^ Answer it should give
   -> String -- ^ Helpful name for this test
   -> IO (Maybe String)
ver t = test (askSMT t)

-- | Test that some action produces an output.
test :: (Monad m, Eq a, Show a)
     => m a -- ^ Action to test
     -> a -- ^ Output it should produce
     -> String -- ^ Helpful name for this test
     -> m (Maybe String)
test t c s = do t' <- t
                -- r <- (== c) <$> t
                if t' == c
                   then return Nothing
                   else return $ Just ("Failure: " ++ s ++ " : " ++ show t')

main :: IO ()
main = report tests

report :: [IO (Maybe String)] -> IO ()
report rs = do fails <- catMaybes <$> sequence rs
               mapM_ print fails
               if fails == []
                  then print "All tests passed."
                  else die "Some tests failed."
