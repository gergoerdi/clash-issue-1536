{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module Hardware.SpaceInvaders
    ( mainBoard
    , topEntity
    ) where

import Clash.Prelude
import Clash.Annotations.TH

import Hardware.Intel8080.CPU

import RetroClash.Utils
import RetroClash.Clock
import RetroClash.Memory
import RetroClash.Barbies
import Data.Maybe

topEntity
    :: "CLK_25MHZ" ::: Clock System
    -> "RESET"     ::: Reset System
    -> "VGA"       ::: Signal System (Unsigned 8)
topEntity = withEnableGen $ board (pure () :: Signal System ())
  where
    board _ = fromMaybe 0 <$> vidWrite
      where
        vidRead = pure Nothing
        (vidAddr, vidWrite) = mainBoard vidRead

mainBoard
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Unsigned 8))
    -> ( Signal dom (Maybe (Index 7168))
      , Signal dom (Maybe (Unsigned 8))
      )
mainBoard vidRead = (vidAddr, vidWrite)
  where
    CPUOut{..} = intel8080 CPUIn{..}

    interruptRequest = pure False

    (dataIn, (vidAddr, vidWrite)) = memoryMap _addrOut _dataOut $ do
        rom <- romFromFile (SNat @0x2000) "_build/SpaceInvaders.bin"
        ram <- ram0 (SNat @0x0400)
        (vid, vidAddr, vidWrite) <- conduit vidRead

        matchRight $ do
            from 0x0000 $ connect rom
            from 0x2000 $ connect ram
            from 0x2400 $ connect vid
            from 0x4000 $ connect ram

        return (vidAddr, vidWrite)

makeTopEntity 'topEntity
