module Board where

import Clash.Prelude
import RetroClash.Memory

import Control.Monad (guard, mplus)
import Data.Kind
import Data.Dependent.Map as DMap
import Data.Dependent.Sum as DSum
import Data.GADT.Compare
import Type.Reflection

topEntity
    :: Clock System
    -> Reset System
    -> Signal System (Maybe (Either (Unsigned 8) (Unsigned 16)))
    -> Signal System (Maybe (Unsigned 8))
    -> Signal System (Maybe (Unsigned 8))
topEntity clk rst addr wr = withClockResetEnable clk rst enableGen $ board addr wr

board
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe (Either (Unsigned 8) (Unsigned 16)))
    -> Signal dom (Maybe (Unsigned 8))
    -> Signal dom (Maybe (Unsigned 8))
board addr wr = dataIn
  where
    conns0 = DMap.fromList
        [ Component (typeRep @(Index 0x2000)) 0 :=> fanInMaybe addr0
        , Component (typeRep @(Index 0x0400)) 1 :=> fanInMaybe addr1
        ]
      where
        addr0 = fmap (match0 =<<) addr
        addr1 = fmap (match1 =<<) addr

        match0 addr = do
            Right addr <- return addr
            guard $ addr < 0x2000
            return $ fromIntegral addr

        match1 addr = do
            Right addr <- return addr
            let match1 = do
                    guard $ 0x2000 <= addr
                    guard $ addr < 0x2400
                    return $ fromIntegral $ addr - 0x2000
                match2 = do
                    guard $ 0x4000 <= addr
                    guard $ addr < 0x4400
                    return $ fromIntegral $ addr - 0x4000
            match1 `mplus` match2

    (dataIn, ()) = memoryMap addr wr $ do
        -- ram <- ram0 (SNat @0x0400)

        -- matchRight $ do
        --     from 0x2000 $ connect ram
        --     from 0x4000 $ connect ram
        () <- return ()
        return ()
