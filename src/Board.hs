{-# LANGUAGE CPP #-}
module Board where

import Clash.Prelude
import RetroClash.Port
import RetroClash.Memory

peripherals
    :: forall dom. HiddenClockResetEnable dom
    => Signal dom (BitVector 8)
    -> Signal dom (Maybe (PortCommand (Index 7) (Unsigned 8)))
    -> Signal dom (Maybe (Unsigned 8))
peripherals dips cmd = pure Nothing

#define ROM 1
#define RAM 1
#define VID 0
#define PORT 0

topEntity
    :: (HiddenClockResetEnable System)
    => Signal System (BitVector 8)
    -> Signal System (Maybe (Unsigned 8))
    -> Signal System (Maybe (Either (Unsigned 8) (Unsigned 16)))
    -> Signal System (Maybe (Unsigned 8))
    -> (Signal System (Maybe (Unsigned 8)), (Signal System (Maybe (Index 7168)), Signal System (Maybe (Unsigned 8))))
topEntity dips vidRead addr wr = memoryMap addr wr $ do
#if ROM
    rom <- romFromFile (SNat @0x2000) "_build/SpaceInvaders.bin"
#endif
#if RAM
    ram <- ram0 (SNat @0x0400)
#endif
#if VID
    (vid, vidAddr, vidWrite) <- conduit vidRead
#else
    let vidAddr = pure Nothing
        vidWrite = pure Nothing
#endif

#if PORT
    io <- port_ $ peripherals dips

    matchLeft $ do
        from 0x00 $ connect io
#endif

    matchRight $ do
#if ROM
        from 0x0000 $ connect rom
#endif
#if RAM
        from 0x2000 $ connect ram
#endif
#if VID
        from 0x2400 $ connect vid
#endif
#if RAM
        from 0x4000 $ connect ram
#endif

    return (vidAddr, vidWrite)
