module Pragmata.Clash.SimScript.Utils (
  execUntil
) where

import Clash.Prelude
import Pragmata.Clash.SimScript.SimScript

  -- Public utilities

-- | Execute the command until result satisifies specified condition
--   return value is pair of result (which satisfies condiotn) and
--   simulation time when is condition satisfied
execUntil :: SimExecFn c r w s          -- ^ exec function
          -> (r -> Bool)                -- ^ condition
          -> c                          -- ^ command
          -> SimScript_ c r w s (r, Int)
execUntil exec test c = do
  rv <- exec c
  t <- getSimTime
  if test rv then return (rv, t)
             else execUntil exec test c


