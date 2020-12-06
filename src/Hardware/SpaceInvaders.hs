{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module Hardware.SpaceInvaders
    ( Player(..)
    , mainBoard
    , topEntity
    ) where

import Clash.Prelude
import Clash.Annotations.TH

import Hardware.SpaceInvaders.Peripherals
import Hardware.Intel8080.CPU
import Hardware.Intel8080.Interruptor

import RetroClash.Utils
import RetroClash.Clock
import RetroClash.PS2
import RetroClash.Memory
import RetroClash.Barbies
import Data.Maybe

topEntity
    :: "CLK_25MHZ" ::: Clock System
    -> "RESET"     ::: Reset System
    -> "PS2"       ::: PS2 System
    -> "VGA"       ::: Signal System (Unsigned 8)
topEntity = withEnableGen board
  where
    board ps2 = fromMaybe 0 <$> vidWrite
      where
        sc = parseScanCode . decodePS2 . samplePS2 $ ps2

        dips = pure 0x00
        tilt = pure False
        coin = keyState 0x021 sc -- 'C'

        p1 = bbundle $ MkPlayer
            { pLeft  = keyState 0x16b sc -- Left arrow
            , pRight = keyState 0x114 sc -- Right arrow
            , pShoot = keyState 0x014 sc -- Left Ctrl
            , pStart = keyState 0x05a sc -- Enter
            }
        p2 = bbundle $ MkPlayer
            { pLeft = pure False
            , pRight = pure False
            , pShoot = pure False
            , pStart = pure False
            }

        vidRead = pure Nothing
        (vidAddr, vidWrite) = mainBoard dips tilt coin p1 p2 vidRead

mainBoard
    :: (HiddenClockResetEnable dom)
    => Signal dom (BitVector 8)
    -> Signal dom Bool
    -> Signal dom Bool
    -> Signal dom (Pure Player)
    -> Signal dom (Pure Player)
    -> Signal dom (Maybe (Unsigned 8))
    -> ( Signal dom (Maybe (Index 7168))
      , Signal dom (Maybe (Unsigned 8))
      )
mainBoard dips tilt coin p1 p2 vidRead = (vidAddr, vidWrite)
  where
    CPUOut{..} = intel8080 CPUIn{..}

    interruptRequest = pure False

    (dataIn, (vidAddr, vidWrite)) = memoryMap _addrOut _dataOut $ do
        rom <- romFromFile (SNat @0x2000) "_build/SpaceInvaders.bin"
        ram <- ram0 (SNat @0x0400)
        (vid, vidAddr, vidWrite) <- conduit vidRead
        io <- port_ $ peripherals dips tilt coin p1 p2

        matchLeft $ do
            from 0x00 $ connect io

        matchRight $ do
            from 0x0000 $ connect rom
            from 0x2000 $ connect ram
            from 0x2400 $ connect vid
            from 0x4000 $ connect ram

        return (vidAddr, vidWrite)

makeTopEntity 'topEntity
