module Main where

import System.Exit
import Data.Maybe

import Z3.Monad

import DSV
import DSV.Effect
import DSV.Contract
import DSV.Effect.Bank

main :: IO ()
main = report tests

tests :: [IO (Maybe String)]
tests = 
  [ok "safe of Dp" (safe nn Dp)
  ,no "safe of Wd" (safe nn Wd)
  ,ok "seqsafe of Dp" (seqsafe nn Dp)
  ,ok "seqsafe of Wp" (seqsafe nn Wd)
  ,no "seqsafe of Bad Wp" (seqsafe nn (Bad Wd))
  ,ok "joint safe Dp and seqsafe Wd" 
      (do dp' <- safe nn Dp
          wd' <- seqsafe nn Wd
          mkAnd [dp',wd'])
  ,no "joint seqsafe Dp and safe Wd" 
      (do dp' <- seqsafe nn Dp
          wd' <- safe nn Wd
          mkAnd [dp',wd'])
  ,ok "consafe of Dp" (consafe c nn Dp)
  ,ok "consafe of Wd" (consafe c nn Wd)
  ,no "consafe of Bad Wd" (consafe c' nn (Bad Wd))
  ,ok "joint consafe Dp and consafe Wd" 
      (do dp' <- consafe c nn Dp
          wd' <- consafe c nn Wd
          mkAnd [dp',wd'])
  ,ok "good bank, good contract" 
      (program (allEffects::[Bank]) c nn) 

  ,no "good bank, empty contract" 
      (program (allEffects::[Bank]) cE nn)

  ,ok "good bank, over-strong contract" 
      (program (allEffects::[Bank]) cS nn)

  ,no "bad bank, good contract" 
      (program (allEffects::[BadBank]) c' nn)

  ,no "bad bank, over-strong contract" 
      (program (allEffects::[BadBank]) cS nn)]
  where c = bankC
        c' = badBankC
        cE = emptyC
        cS = strongC
        nn = nonNegative
        
ok :: String -> Z3 AST -> IO (Maybe String)
ok s p = test (verify p) True s

no :: String -> Z3 AST -> IO (Maybe String)
no s p = test (verify p) False ("[NOT] " ++ s)

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
