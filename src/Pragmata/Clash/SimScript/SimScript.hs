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
-- import Debug.Trace (trace)

import Pragmata.Clash.SimScript.Orphans ()

-- = Types

-- | Top level simulation script
type SimScript c r w   = SimScript_ c r w (CR c r)
-- | Simulation script
type SimScript_ c r w v  = ContT (CR c r) (State (CC c r w, w, Int)) v
-- | Type of simlation script exec function
type SimExecFn c r w = (c -> SimScript_ c r w r)
-- | Type of simlation script continue function
type SimContinueFn c r w = SimScript_ c r w ()

data CR c r = C c | R r | A
  deriving (Generic, NFDataX, Show)

newtype CC c r w = MkCC { runCC :: SimScript c r w }

deriving newtype instance (NFDataX c, NFDataX r, NFDataX w) => NFDataX (State (CC c r w, w, Int) (CR c r))
deriving newtype instance (NFDataX c, NFDataX r, NFDataX w) => NFDataX (SimScript c r w)
deriving newtype instance (NFDataX c, NFDataX r, NFDataX w) => NFDataX (CC c r w)

data ExecState c r w
  = ESNotStarted { esSimScript :: CC c r w }
  | ESStarted    { esSimScript :: CC c r w, esLog :: w, esPrevCmd :: c, esTime :: Int }
  | ESStopped    { esLog :: w, esPrevCmd :: c }
  deriving (Generic, NFDataX)

script :: Monoid w
       => ((c -> SimScript_ c r w r) -> SimScript c r w -> SimScript c r w)
       -> SimScript c r w
script pp = callCC $ \k -> pp (execute k) (exitScript k)

-- | Run execution script and return log
execScriptN :: (KnownDomain dom, NFDataX c, NFDataX r, NFDataX w, Monoid w)
            => Int
            -> (HiddenClockResetEnable dom => Signal dom c -> Signal dom r)
            -> SimScript c r w -> w
execScriptN n circ sc = L.last $ snd <$> sampleN n (runSimScript circ sc)

-- | Run execution script and return list of simulation results
evalScriptN :: (KnownDomain dom, NFDataX c, NFDataX r, NFDataX w, Monoid w)
            => Int
            -> (HiddenClockResetEnable dom => Signal dom c -> Signal dom r)
            -> SimScript c r w
            -> [r]
evalScriptN n circ sc = fst <$> sampleN n (runSimScript circ sc)

-- | Run execution script and return pair (list of simulation results, log)
runScriptN :: (KnownDomain dom, NFDataX c, NFDataX r, NFDataX w, Monoid w)
           => Int
           -> (HiddenClockResetEnable dom => Signal dom c -> Signal dom r)
           -> SimScript c r w
           -> ([r], w)
runScriptN n circ sc = let c = sampleN n (runSimScript circ sc) in (fst <$> c, L.last (snd <$> c))

-- | Append to simulation log
tell :: (Monoid w) => w -> SimScript_ c r w ()
tell l = do
  modify (\(s, l', c) -> (s, l' <> l, c))

-- | Returns the current simulation time (clock ticks)
getSimTime :: SimScript_ c r w Int
getSimTime = do
  (_, _, c) <- get
  return c

-- Private fns: Simulation and interpreter

runSimScript :: forall dom c r w. (HiddenClockResetEnable dom, NFDataX c, NFDataX r, NFDataX w, Monoid w)
           => (Signal dom c -> Signal dom r)
           -> SimScript c r w
           -> Signal dom (r, w)
runSimScript crc scr = bundle (res, l)
  where
    res = crc cmd
    (cmd, l) = unbundle $ mealy interpreter (ESNotStarted (MkCC scr)) res


interpreter :: (Monoid w) => ExecState c r w -> r -> (ExecState c r w, (c, w))
interpreter (ESNotStarted c) r = res
  where
    (rs, (c', l', _)) = runState (runContT (runCC c) return) (MkCC (return (R r)), mempty, 0)
    res = case rs of
      C cmd' -> (ESStarted c' l' cmd' 0, (cmd', l'))
      _      -> error "A simulation must have at least one step!"

interpreter (ESStarted c l cmd t) r = res
  where
    (d, (c', l', _)) = runState (runContT (runCC c) return) (MkCC (return (R r)), l, t + 1)
    res = case d of
            C cmd' -> (ESStarted c' l' cmd' (t + 1), (cmd', l'))
            A      -> (ESStopped l' cmd, (cmd, l'))
            _      -> error "Error in SimScript Protocol (tfn)"

interpreter s@(ESStopped l cmd) _ = (s, (cmd, l))

-- Private functions: Script side

execute :: (Monoid w) => (CR c r -> SimScript c r w) -> c -> SimScript_ c r w r
execute k v = do
  sendCommand k (C v)
  readResponse

exitScript :: (Monoid w) => (CR c r -> SimScript c r w) -> SimScript c r w
exitScript k = do
  sendCommand k A
  return A

sendCommand :: (Monoid w) => (CR c r -> SimScript c r w) -> CR c r -> SimScript_ c r w ()
sendCommand kk v = callCC $ \k -> do
  modify $ \(_, l, c) -> (MkCC (k undefined), l, c)     -- k never be called
  void $ kk v                                           -- abort current continuation

readResponse :: (Monoid w) => SimScript_ c r w r
readResponse = do
  (MkCC res, _, _) <- get
  rv <- res
  case rv of
    R v -> return v
    _   -> error "Error in SimScript Protocol (readResponse)"
