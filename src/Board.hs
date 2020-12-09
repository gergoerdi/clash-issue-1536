module Board where

import Clash.Prelude
import RetroClash.Memory

topEntity
    :: Clock System
    -> Reset System
    -> Signal System (Maybe (Unsigned 16))
    -> Signal System (Maybe (Unsigned 8))
    -> (Signal System (Maybe (Unsigned 8)), ())
topEntity clk rst addr wr = withClockResetEnable clk rst enableGen $ memoryMap addr wr $ do
    -- ram <- ram0 (SNat @0x0400)

    -- from 0x2000 $ connect ram
    -- from 0x4000 $ connect ram

    () <- return ()
    return ()
