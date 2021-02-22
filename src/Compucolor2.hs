{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module Compucolor2 where

import Clash.Prelude
import RetroClash.Port
import RetroClash.Memory

type VidAddr = Index 7168

tms5501
    :: Signal dom (Maybe (PortCommand (Index 0x10) (Unsigned 8)))
    -> (Signal dom (Maybe (Unsigned 8)), Signal dom ())
tms5501 _ = (pure Nothing, pure ())

crt5027
    :: Signal dom (Maybe (PortCommand (Index 0x10) (Unsigned 8)))
    -> (Signal dom (Maybe (Unsigned 8)), Signal dom ())
crt5027 _ = (pure Nothing, pure ())

topEntity
    :: (HiddenClockResetEnable System)
    => Signal System (Maybe (Unsigned 8))
    -> Signal System (Maybe (Either (Unsigned 8) (Unsigned 16)))
    -> Signal System (Maybe (Unsigned 8))
    -> ( Signal System ()
       , Signal System (Maybe (Bool, VidAddr))
       , Signal System (Maybe (Unsigned 8))
       )
topEntity vidRead addr wr = (crtOut, vidAddr, vidWrite)
  where
    (dataIn, (crtOut, tmsOut, (vidAddr, vidWrite))) =
        memoryMap addr wr $ do
            rom <- romFromFile (SNat @0x4000) "_build/v678.rom.bin"
            ram <- ram0 (SNat @0x8000)
            (vid, vidAddr, vidWrite) <- conduit vidRead

            (tms, tmsOut) <- port tms5501
            (crt, crtOut) <- port crt5027
            prom <- readWrite_ @(Index 0x20) (\_ _ -> pure $ Just 0x00) -- TODO

            -- override rst

            matchLeft $ do
                from 0x00 $ connect tms
                from 0x10 $ connect tms
                from 0x60 $ connect crt
                from 0x70 $ connect crt
                from 0x80 $ connect prom

            matchRight $ do
                from 0x0000 $ connect rom
                from 0x6000 $ tag True $ connect vid
                from 0x7000 $ tag False $ connect vid
                from 0x8000 $ connect ram

            return (crtOut, tmsOut, (vidAddr, vidWrite))
