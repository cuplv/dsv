module Main where

import System.Exit
import Data.Maybe

import Z3.Monad

import DSV
import DSV.Effect
import DSV.Effect.Bank

main :: IO ()
main = report tests

tests :: [IO (Maybe String)]
tests = 
  [test' (mkNot =<< safe nn Dp) Unsat "safe of Dp"
  ,test' (mkNot =<< safe nn Wd) Sat "(not) safe of Wd"
  ,test' (mkNot =<< seqsafe nn Wd) Unsat "seqsafe of Dp"
  ,test' (do dp' <- mkNot =<< safe nn Dp
             wd' <- mkNot =<< seqsafe nn Wd
             mkOr [dp',wd']) Unsat "joint safe Dp and seqsafe Wd"
  ,test' (do dp' <- mkNot =<< seqsafe nn Dp
             wd' <- mkNot =<< safe nn Wd
             mkOr [dp',wd']) Sat "(not) joint seqsafe Dp and safe Wd"
  ,test' (mkNot =<< consafe nn Dp) Unsat "consafe of Dp"
  ,test' (mkNot =<< consafe nn Wd) Unsat "consafe of Wd"
  ,test' (do dp' <- mkNot =<< consafe nn Dp
             wd' <- mkNot =<< consafe nn Wd
             mkOr [dp',wd']) Unsat "joint consafe Dp and consafe Wd"]

test' t c s = test (quickEval t) c s

nn = nonNegative

quickEval :: Z3 AST -> IO Result
quickEval p = evalZ3 (p >>= assert >> check)

test :: (Monad m, Eq a) => m a -> a -> String -> m (Maybe String)
test t c s = do r <- (== c) <$> t
                if r
                   then return Nothing
                   else return $ Just ("Failure: " ++ s)

report :: [IO (Maybe String)] -> IO ()
report rs = do fails <- catMaybes <$> sequence rs
               mapM_ print fails
               if fails == []
                  then print "All tests passed."
                  else die "Some tests failed."
