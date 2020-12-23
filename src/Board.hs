{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Board where

import Clash.Prelude
import Data.Maybe
import Control.Monad
import Control.Monad.RWS

import RetroClash.Memory

topEntity
    :: (HiddenClockResetEnable System)
    => Signal System (Maybe (Index 0x0400))
    -> Signal System (Maybe (Unsigned 8))
    -> (Signal System (Maybe (Unsigned 8)), ())
topEntity addr wr = memoryMap addr wr $ do
    ram <- ram0 (SNat @0x0400)
    from 0x0000 $ connect ram
    from 0x0400 $ connect ram
