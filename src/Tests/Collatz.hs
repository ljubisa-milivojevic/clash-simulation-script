module Tests.Collatz where

import Clash.Prelude
import Pragmata.Clash.SimScript.SimScript
import Control.Monad ( forM_ )

-- | Circuit for testing:
--   for a given number, compute the next element in Collatz the sequence
collatz :: forall dom. (HiddenClockResetEnable dom)
      => Signal dom (Unsigned 32)
      -> Signal dom (Unsigned 32)
collatz v = register 0 (run <$> v)
  where
    run :: Unsigned 32 -> Unsigned 32
    run 1 = 0
    run x | even x    = x `div` 2
          | otherwise = x * 3 + 1

-- | Tests
tests :: [Unsigned 32]
tests = [5, 9, 12]

-- | Main test (simulation) script
--   for each test in tests calls subscript execSteps
testScript :: SimScript (Unsigned 32) (Unsigned 32) [String]
testScript = script $ \exec continue -> do
  forM_ tests $ \v -> do
    tell["Computing sequence for " <> show v]
    execSteps exec v
  continue

-- | Executes Collatz computation steps until reaching value 1
execSteps :: SimExecFn (Unsigned 32) (Unsigned 32) [String] -> Unsigned 32 -> SimScript_ (Unsigned 32) (Unsigned 32) [String] ()
execSteps _    1 = tell["1"]
execSteps exec v = do
  tell[show v]
  -- exec actualy "calls" collatz coputation
  nv <- exec v
  execSteps exec nv

-- >>> execScriptN @System 50 collatz testScript
-- ["Computing sequence for 5","5","16","8","4","2","1","Computing sequence for 9","9","28","14","7","22","11","34","17","52","26","13","40","20","10","5","16","8","4","2","1","Computing sequence for 12","12","6","3","10","5","16","8","4","2","1"]
