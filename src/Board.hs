module Board where

import Clash.Prelude
import RetroClash.Memory

topEntity
    :: (HiddenClockResetEnable System)
    => Signal System (Maybe (Unsigned 16))
    -> Signal System (Maybe (Unsigned 8))
    -> Signal System (Maybe (Unsigned 8))
topEntity addr wr = memoryMap_ addr wr $ do
    ram <- ram0 (SNat @0x0400)
    -- return ()

    from 0x2000 $ connect ram
    from 0x4000 $ connect ram
