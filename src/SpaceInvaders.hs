{-# LANGUAGE CPP #-}
module SpaceInvaders where

import Clash.Prelude
import RetroClash.Port
import RetroClash.Memory

peripherals
    :: forall dom. HiddenClockResetEnable dom
    => Signal dom (BitVector 8)
    -> Signal dom (Maybe (PortCommand (Index 7) (Unsigned 8)))
    -> Signal dom (Maybe (Unsigned 8))
peripherals dips cmd = pure Nothing

topEntity
    :: (HiddenClockResetEnable System)
    => Signal System (BitVector 8)
    -> Signal System (Maybe (Unsigned 8))
    -> Signal System (Maybe (Either (Unsigned 8) (Unsigned 16)))
    -> Signal System (Maybe (Unsigned 8))
    -> (Signal System (Maybe (Unsigned 8)), (Signal System (Maybe (Index 7168)), Signal System (Maybe (Unsigned 8))))
topEntity dips vidRead addr wr = memoryMap addr wr $ do
    rom <- romFromFile (SNat @0x2000) "_build/SpaceInvaders.bin"
    ram <- ram0 (SNat @0x0400)
    (vid, vidAddr, vidWrite) <- conduit vidRead

    io <- port_ $ peripherals dips

    matchLeft $ do
        from 0x00 $ connect io

    matchRight $ do
        from 0x0000 $ connect rom
        from 0x2000 $ connect ram
        from 0x2400 $ connect vid
        from 0x4000 $ connect ram

    return (vidAddr, vidWrite)
