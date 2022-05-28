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
--   For each test in tests calls subscript execSteps
testScript :: SimScript (Unsigned 32) (Unsigned 32) [String] ()
testScript = script $ \exec continue -> do
  forM_ tests $ \v -> do
    tell ["Computing sequence for " <> show v]
    execSteps exec v
  continue

-- | Executes Collatz computation steps until reaching value 1
execSteps :: SimExecFn (Unsigned 32) (Unsigned 32) [String] () -> Unsigned 32 -> SimScript_ (Unsigned 32) (Unsigned 32) [String] () ()
execSteps _    1 = tell ["1"]
execSteps exec v = do
  tell ["sequence item: " <> show v]
  -- exec actualy "calls" collatz coputation
  nv <- exec v
  execSteps exec nv

-- >>> fst $ execScriptN @System 50 collatz () testScript
-- ["Computing sequence for 5","sequence item: 5","sequence item: 16","sequence item: 8","sequence item: 4","sequence item: 2","1","Computing sequence for 9","sequence item: 9","sequence item: 28","sequence item: 14","sequence item: 7","sequence item: 22","sequence item: 11","sequence item: 34","sequence item: 17","sequence item: 52","sequence item: 26","sequence item: 13","sequence item: 40","sequence item: 20","sequence item: 10","sequence item: 5","sequence item: 16","sequence item: 8","sequence item: 4","sequence item: 2","1","Computing sequence for 12","sequence item: 12","sequence item: 6","sequence item: 3","sequence item: 10","sequence item: 5","sequence item: 16","sequence item: 8","sequence item: 4","sequence item: 2","1"]

-- | Main test (simulation) script for testing simulation
--   with sequence length acumalted in state
--   For each test in tests: reset counter and calls subscript execStepsWithState
testScriptWithState :: SimScript (Unsigned 32) (Unsigned 32) [String] (Unsigned 32)
testScriptWithState = script $ \exec continue -> do
  forM_ tests $ \v -> do
    put 0
    tell ["Computing sequence for " <> show v]
    execStepsWithState exec v
    cnt <- get
    tell ["Computation finished in " <> show cnt <> " steps"]
  continue

-- | Executes Collatz computation steps until reaching value 1
--   ounting number of steps in state
execStepsWithState :: SimExecFn (Unsigned 32) (Unsigned 32) [String] (Unsigned 32)
                   -> Unsigned 32
                   -> SimScript_ (Unsigned 32) (Unsigned 32) [String] (Unsigned 32) ()
execStepsWithState _    1 = tell ["sequence item: 1"]
execStepsWithState exec v = do
  tell ["sequence item: " <> show v]
  modify (+ 1)
  -- exec actualy "calls" collatz coputation
  nv <- exec v
  execStepsWithState exec nv

-- >>> fst $ execScriptN @System 50 collatz 0 testScriptWithState
-- ["Computing sequence for 5","sequence item: 5","sequence item: 16","sequence item: 8","sequence item: 4","sequence item: 2","sequence item: 1","Computation finished in 5 steps","Computing sequence for 9","sequence item: 9","sequence item: 28","sequence item: 14","sequence item: 7","sequence item: 22","sequence item: 11","sequence item: 34","sequence item: 17","sequence item: 52","sequence item: 26","sequence item: 13","sequence item: 40","sequence item: 20","sequence item: 10","sequence item: 5","sequence item: 16","sequence item: 8","sequence item: 4","sequence item: 2","sequence item: 1","Computation finished in 19 steps","Computing sequence for 12","sequence item: 12","sequence item: 6","sequence item: 3","sequence item: 10","sequence item: 5","sequence item: 16","sequence item: 8","sequence item: 4","sequence item: 2","sequence item: 1","Computation finished in 9 steps"]
