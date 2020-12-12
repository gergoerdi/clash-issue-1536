{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Board where

import Clash.Prelude
import Data.Maybe
import Control.Monad
import Control.Monad.RWS

import Unsafe.Coerce
import Data.Map as Map

newtype Component s addr = Component Int
    deriving newtype (Eq, Ord)

newtype FanIn dom a = FanIn{ getFanIn :: Signal dom `Ap` First a }
    deriving newtype (Semigroup, Monoid)

newtype AddrMap s dom = AddrMap{ addrMap :: Map Int (FanIn dom ()) }
    deriving newtype (Monoid)

instance Semigroup (AddrMap s dom) where
    AddrMap map1 <> AddrMap map2 = AddrMap $ unionWithKey (const mappend) map1 map2

newtype Addressing s dom dat addr a = Addressing
    { unAddressing :: RWS
          (FanIn dom addr, AddrMap s dom)
          (FanIn dom (Maybe dat), AddrMap s dom)
          Int
          a
    }
    deriving newtype (Functor, Applicative, Monad)

memoryMap
    :: Signal dom (Maybe addr)
    -> (forall s. Addressing s dom dat addr a)
    -> (Signal dom (Maybe dat), a)
memoryMap addr body = (join <$> firstIn read, x)
  where
    (x, (read, conns)) = evalRWS (unAddressing body) (fanInMaybe addr, conns) 0

readWrite_
    :: (HiddenClockResetEnable dom)
    => (Signal dom (Maybe addr') -> Signal dom (Maybe dat))
    -> Addressing s dom dat addr (Component s addr')
readWrite_ mkComponent = Addressing $ do
    component@(Component i) <- Component <$> get <* modify succ
    (_, addrs) <- ask
    let addr = firstIn . fromMaybe mempty $ Map.lookup i (addrMap addrs)
        read = mkComponent $ unsafeCoerce addr
    tell (fanIn read, mempty)
    return component

ram0
    :: (HiddenClockResetEnable dom, 1 <= n, NFDataX dat, Num dat)
    => SNat n
    -> Addressing s dom dat addr (Component s (Index n))
ram0 size@SNat = readWrite_ $ \addr ->
    fmap Just $ blockRam1 ClearOnReset size 0 (fromMaybe 0 <$> addr) (pure Nothing)

connect
    :: Component s addr
    -> Addressing s dom dat addr ()
connect component@(Component i) = Addressing $ do
    (addr, _) <- ask
    tell (mempty, AddrMap $ Map.singleton i $ unsafeCoerce addr)

firstIn :: FanIn dom a -> Signal dom (Maybe a)
firstIn = fmap getFirst . getAp . getFanIn

fanInMaybe :: Signal dom (Maybe a) -> FanIn dom a
fanInMaybe = FanIn . Ap . fmap First

fanIn :: Signal dom a -> FanIn dom a
fanIn = fanInMaybe . fmap pure

topEntity
    :: Clock System
    -> Reset System
    -> Signal System (Maybe (Index 0x0400))
    -> (Signal System (Maybe (Unsigned 8)), ())
topEntity clk rst addr = withClockResetEnable clk rst enableGen $ memoryMap addr $ do
    ram <- ram0 (SNat @0x0400)
    connect ram
