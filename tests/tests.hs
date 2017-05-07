module Main where

import System.Exit
import Data.Maybe

import Language.SMTLib2
import DSV.Prelude
import DSV.Examples.Bank
import DSV.Examples.ThreeBools

main :: IO ()
main = report (tests ++ boolTests)

boolTests :: [IO (Maybe String)]
boolTests = 
  [no "safe of S1" (safe na S1)
  ,no "safe of S2" (safe na S2)
  ,no "safe of S3" (safe na S3)] ++
  [ok "seqsafe of S1" (seqsafe na S1)
  ,ok "seqsafe of S2" (seqsafe na S2)
  ,ok "seqsafe of S3" (seqsafe na S3)]
  where na = notAllBools

tests :: [IO (Maybe String)]
tests = 
  [ok "safe of Dp" (safe nn Dp)
  ,no "safe of Wd" (safe nn Wd)
  ,ok "seqsafe of Dp" (seqsafe nn Dp)
  ,ok "seqsafe of Wp" (seqsafe nn Wd)
  ,no "seqsafe of Bad Wp" (seqsafe nn (Bad Wd))
  ,ok "joint safe Dp and seqsafe Wd" (safe nn Dp .&. seqsafe nn Wd)
  ,no "joint seqsafe Dp and safe Wd" (safe nn Wd .&. seqsafe nn Dp)
  ,ok "consafe of Dp" (consafe c nn Dp)
  ,ok "consafe of Wd" (consafe c nn Wd)
  ,no "consafe of Bad Wd" (consafe c' nn (Bad Wd))
  ,ok "joint consafe Dp and consafe Wd" (consafe c nn Dp .&. consafe c nn Wd)
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
        
ok s p = test (verify p) True s
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
