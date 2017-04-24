module DSV where

import qualified Data.Traversable as T
import Data.Maybe

import Z3.Monad

test :: Z3 (Maybe [Integer])
test = do q1 <- mkFreshIntVar "q1"
          q2 <- mkFreshIntVar "q2"
          assert =<< mkAnd =<< T.sequence [mkLt q1 q2]
          fmap snd $ withModel $ \m -> catMaybes <$> mapM (evalInt m) [q1,q2]

runScript s = evalZ3With Nothing opts s >>= \mbSol ->
            case mbSol of
              Nothing -> error "No solution found."
              Just sol -> putStr "Solution: " >> print sol
  where opts = opt "MODEL" True +? opt "MODEL_COMPLETION" True
