{-# LANGUAGE NumericUnderscores, RecordWildCards #-}
module Hardware.SpaceInvaders
    ( Player(..)
    , mainBoard
    , topEntity
    ) where

import Clash.Prelude
import Clash.Annotations.TH

import Hardware.SpaceInvaders.Video
import Hardware.SpaceInvaders.Peripherals
import Hardware.Intel8080.CPU
import Hardware.Intel8080.Interruptor

import RetroClash.Utils
import RetroClash.Clock
import RetroClash.VGA
import RetroClash.PS2
import RetroClash.Memory
import RetroClash.Barbies
import Data.Maybe
import Control.Monad (guard, mplus)

import Data.Dependent.Map as DMap
import Data.Dependent.Sum as DSum
import Type.Reflection


topEntity
    :: "CLK_25MHZ" ::: Clock Dom25
    -> "RESET"     ::: Reset Dom25
    -> "PS2"       ::: PS2 Dom25
    -> "VGA"       ::: VGAOut Dom25 8 8 8
topEntity = withEnableGen board
  where
    board ps2 = vga
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

        (vga, vidRead, lineEnd) = video (fromMaybe 0 <$> vidAddr) vidWrite
        (vidAddr, vidWrite) = mainBoard dips tilt coin p1 p2 vidRead lineEnd

mainBoard
    :: (HiddenClockResetEnable dom)
    => Signal dom (BitVector 8)
    -> Signal dom Bool
    -> Signal dom Bool
    -> Signal dom (Pure Player)
    -> Signal dom (Pure Player)
    -> Signal dom (Maybe (Unsigned 8))
    -> Signal dom (Maybe (Index VidY))
    -> ( Signal dom (Maybe VidAddr)
      , Signal dom (Maybe (Unsigned 8))
      )
mainBoard dips tilt coin p1 p2 vidRead lineEnd = (vidAddr, vidWrite)
  where
    CPUOut{..} = intel8080 CPUIn{..}

    (interruptRequest, rst) = interruptor irq (delay False _interruptAck)
    irq = muxA
        [ enable (lineEnd .== Just 95) (pure 1)
        , enable (lineEnd .== Just maxBound) (pure 2)
        ]

    conns0 = DMap.fromList
        [ Component (typeRep :: TypeRep (Index 0x2000)) 0 :=> fanInMaybe addr0
        , Component (typeRep :: TypeRep (Index 0x0400)) 1 :=> fanInMaybe addr1
        , Component (typeRep :: TypeRep VidAddr) 2 :=> fanInMaybe addr2
        , Component (typeRep :: TypeRep (Index 7)) 3 :=> fanInMaybe addr3
        ]
      where
        addr0 = fmap (match0 =<<) _addrOut
        addr1 = fmap (match1 =<<) _addrOut
        addr2 = fmap (match2 =<<) _addrOut
        addr3 = fmap (match3 =<<) _addrOut

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

        match2 addr = do
            Right addr <- return addr
            guard $ 0x2400 <= addr
            guard $ addr < 0x4000
            return $ fromIntegral $ addr - 0x2400

        match3 addr = do
            Left addr <- return addr
            guard $ addr <= 7
            return $ fromIntegral addr

    (dataIn, (vidAddr, vidWrite)) = memoryMap _addrOut _dataOut conns0 $ override rst $ do
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
