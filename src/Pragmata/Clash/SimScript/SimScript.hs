{- |
  Copyright   : Pragmata - Belgrade
  Licence     : BSD2
  Maintainer  : Ljubisa Milivojevic <ljubisa_milivojevic@yahoo.com>
  Stability   : experimental

  Using the simulation script for testing clash circuits
-}

{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# OPTIONS_HADDOCK not-home #-}

module Pragmata.Clash.SimScript.SimScript (
  -- * Types
  SimScript, SimScript_, SimExecFn, SimContinueFn,
  -- * Creating script
  script,
  -- * Executing simulation
  execScriptN, evalScriptN, runScriptN,
  -- * Basic script's function
  tell, getSimTime,
) where

import Clash.Prelude

import Control.Monad.Cont
import Control.Monad.State
import qualified Data.List as L
import Data.Functor.Identity (Identity(Identity))
-- import Debug.Trace (trace)

-- = Types

-- | Top level simulation script
type SimScript c r w s  = SimScript_ c r w s (CR c r)
-- | Simulation script
type SimScript_ c r w s v  = ContT (CR c r) (State (CC c r w s, w, s, Int)) v
-- | Type of simlation script exec function
type SimExecFn c r w s = (c -> SimScript_ c r w s r)
-- | Type of simlation script continue function
type SimContinueFn c r w s = SimScript_ c r w s ()

data CR c r = C c | R r | A
  deriving (Generic, NFDataX, Show)

newtype CC c r w s = MkCC { runCC :: SimScript c r w s }

deriving newtype instance (NFDataX c, NFDataX r, NFDataX w, NFDataX s) => NFDataX (Identity (CR c r, (CC c r w s, w, s, Int)))
deriving newtype instance (NFDataX c, NFDataX r, NFDataX w, NFDataX s) => NFDataX (State (CC c r w s, w, s, Int) (CR c r))
deriving newtype instance (NFDataX c, NFDataX r, NFDataX w, NFDataX s) => NFDataX (SimScript c r w s)
deriving newtype instance (NFDataX c, NFDataX r, NFDataX w, NFDataX s) => NFDataX (CC c r w s)

data ExecState c r w s
  = ESNotStarted { esSimScript :: CC c r w s, esUserSate :: s }
  | ESStarted    { esSimScript :: CC c r w s, esLog :: w, esUserSate :: s, esPrevCmd :: c, esTime :: Int }
  | ESStopped    { esLog :: w, esUserSate :: s, esPrevCmd :: c }
  deriving (Generic, NFDataX)

script :: Monoid w
       => ((c -> SimScript_ c r w s r) -> SimScript c r w s -> SimScript c r w s)
       -> SimScript c r w s
script pp = callCC $ \k -> pp (execute k) (exitScript k)

-- | Run execution script and return log
execScriptN :: (KnownDomain dom, NFDataX c, NFDataX r, NFDataX w, NFDataX s, Monoid w)
            => Int
            -> (HiddenClockResetEnable dom => Signal dom c -> Signal dom r)
            -> s
            -> SimScript c r w s -> (w, s)
execScriptN n circ s sc = L.last $ snd <$> sampleN n (runSimScript circ s sc)

-- | Run execution script and return list of simulation results
evalScriptN :: (KnownDomain dom, NFDataX c, NFDataX r, NFDataX w, NFDataX s, Monoid w)
            => Int
            -> (HiddenClockResetEnable dom => Signal dom c -> Signal dom r)
            -> s
            -> SimScript c r w s
            -> [r]
evalScriptN n circ s sc = fst <$> sampleN n (runSimScript circ s sc)

-- | Run execution script and return pair (list of simulation results, log)
runScriptN :: (KnownDomain dom, NFDataX c, NFDataX r, NFDataX w, NFDataX s, Monoid w)
           => Int
           -> (HiddenClockResetEnable dom => Signal dom c -> Signal dom r)
           -> s
           -> SimScript c r w s
           -> ([r], (w, s))
runScriptN n circ s sc = let c = sampleN n (runSimScript circ s sc) in (fst <$> c, L.last (snd <$> c))

-- | Append to simulation log
tell :: (Monoid w) => w -> SimScript_ c r w s ()
tell l = do
  modify (\(s, l', us, c) -> (s, l' <> l, us, c))

-- | Returns the current simulation time (clock ticks)
getSimTime :: SimScript_ c r w s Int
getSimTime = do
  (_, _, _, c) <- get
  return c

-- Private fns: Simulation and interpreter

runSimScript :: forall dom c r w s. (HiddenClockResetEnable dom, NFDataX c, NFDataX r, NFDataX w, NFDataX s, Monoid w)
           => (Signal dom c -> Signal dom r)
           -> s
           -> SimScript c r w s
           -> Signal dom (r, (w, s))
runSimScript crc s scr = bundle (res, bundle (l', s'))
  where
    res = crc cmd
    (cmd, l', s') = unbundle $ mealy interpreter (ESNotStarted (MkCC scr) s) res


interpreter :: (Monoid w) => ExecState c r w s -> r -> (ExecState c r w s, (c, w, s))
interpreter (ESNotStarted c s) r = res
  where
    (rs, (c', l', s', _)) = runState (runContT (runCC c) return) (MkCC (return (R r)), mempty, s, 0)
    res = case rs of
      C cmd' -> (ESStarted c' l' s' cmd' 0, (cmd', l', s'))
      _      -> error "A simulation must have at least one step!"

interpreter (ESStarted c l s cmd t) r = res
  where
    (d, (c', l', s', _)) = runState (runContT (runCC c) return) (MkCC (return (R r)), l, s, t + 1)
    res = case d of
            C cmd' -> (ESStarted c' l' s' cmd' (t + 1), (cmd', l', s'))
            A      -> (ESStopped l' s' cmd, (cmd, l', s'))
            _      -> error "Error in SimScript Protocol (tfn)"

interpreter st@(ESStopped l s cmd) _ = (st, (cmd, l, s))

-- Private functions: Script side

execute :: (Monoid w) => (CR c r -> SimScript c r w s) -> c -> SimScript_ c r w s r
execute k v = do
  sendCommand k (C v)
  readResponse

exitScript :: (Monoid w) => (CR c r -> SimScript c r w s) -> SimScript c r w s
exitScript k = do
  sendCommand k A
  return A

sendCommand :: (Monoid w) => (CR c r -> SimScript c r w s) -> CR c r -> SimScript_ c r w s ()
sendCommand kk v = callCC $ \k -> do
  modify $ \(_, l, s, c) -> (MkCC (k undefined), l, s, c)     -- k never be called
  void $ kk v                                           -- abort current continuation

readResponse :: (Monoid w) => SimScript_ c r w s r
readResponse = do
  (MkCC res, _, _, _) <- get
  rv <- res
  case rv of
    R v -> return v
    _   -> error "Error in SimScript Protocol (readResponse)"
