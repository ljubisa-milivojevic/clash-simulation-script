{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Pragmata.Clash.SimScript.Orphans () where

import Clash.Prelude ( NFDataX )
import Data.Functor.Identity ( Identity(Identity) )

deriving newtype instance (NFDataX a) => NFDataX (Identity a)
