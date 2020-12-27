{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module RetroClash.Memory where

import Clash.Prelude
import Control.Arrow (first, second)
import Data.Maybe
import Control.Monad
import Control.Monad.RWS

import Unsafe.Coerce
import Assoc as Map

type Key = Int

newtype Component s addr = Component Key
    deriving newtype (Eq, Ord)

newtype FanIn dom a = FanIn{ getFanIn :: Signal dom `Ap` First a }
    deriving newtype (Semigroup, Monoid)

newtype AddrMap s dom = AddrMap{ addrMap :: Map Key (FanIn dom ()) }
    deriving newtype (Monoid)

instance Semigroup (AddrMap s dom) where
    AddrMap map1 <> AddrMap map2 = AddrMap $ unionWithKey (const mappend) map1 map2

newtype Addressing s dom dat addr a = Addressing
    { unAddressing :: RWS
          (FanIn dom addr, Signal dom (Maybe dat), AddrMap s dom)
          (FanIn dom (Maybe dat), AddrMap s dom)
          Key
          a
    }
    deriving newtype (Functor, Applicative, Monad)

memoryMap
    :: Signal dom (Maybe addr)
    -> Signal dom (Maybe dat)
    -> (forall s. Addressing s dom dat addr a)
    -> (Signal dom (Maybe dat), a)
memoryMap addr wr body = (join <$> firstIn read, x)
  where
    (x, (read, conns)) = evalRWS (unAddressing body) (fanInMaybe addr, wr, conns) 0

memoryMap_
    :: Signal dom (Maybe addr)
    -> Signal dom (Maybe dat)
    -> (forall s. Addressing s dom dat addr ())
    -> Signal dom (Maybe dat)
memoryMap_ addr wr body = fst $ memoryMap addr wr body

conduit
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe dat)
    -> Addressing s dom dat addr (Component s addr', Signal dom (Maybe addr'), Signal dom (Maybe dat))
conduit read = do
    (component, (addr, wr)) <- readWrite $ \addr wr -> (read, (addr, wr))
    return (component, addr, wr)

readWrite
    :: (HiddenClockResetEnable dom)
    => (Signal dom (Maybe addr') -> Signal dom (Maybe dat) -> (Signal dom (Maybe dat), a))
    -> Addressing s dom dat addr (Component s addr', a)
readWrite mkComponent = Addressing $ do
    component@(Component i) <- Component <$> get <* modify succ
    (_, wr, addrs) <- ask
    let addr = firstIn . fromMaybe mempty $ Map.lookup i (addrMap addrs)
        selected = isJust <$> addr
        (read, x) = mkComponent (unsafeCoerce addr) wr
    tell (gated (delay False selected) $ fanIn read, mempty)
    return (component, x)

readWrite_
    :: (HiddenClockResetEnable dom)
    => (Signal dom (Maybe addr') -> Signal dom (Maybe dat) -> Signal dom (Maybe dat))
    -> Addressing s dom dat addr (Component s addr')
readWrite_ mkComponent = fmap fst $ readWrite $ \addr wr -> (mkComponent addr wr, ())

romFromFile
    :: (HiddenClockResetEnable dom, 1 <= n, BitPack dat)
    => SNat n
    -> FilePath
    -> Addressing s dom dat addr (Component s (Index n))
romFromFile size@SNat fileName = readWrite_ $ \addr _wr ->
    fmap (Just . unpack) $ romFilePow2 fileName (maybe 0 bitCoerce <$> addr)

ram0
    :: (HiddenClockResetEnable dom, 1 <= n, NFDataX dat, Num dat)
    => SNat n
    -> Addressing s dom dat addr (Component s (Index n))
ram0 size@SNat = readWrite_ $ \addr wr ->
    fmap Just $ blockRam1 ClearOnReset size 0 (fromMaybe 0 <$> addr) (liftA2 (,) <$> addr <*> wr)

matchAddr
    :: (addr -> Maybe addr')
    -> Addressing s dom dat addr' a
    -> Addressing s dom dat addr a
matchAddr match body = Addressing $ rws $ \(addr, wr, addrs) s ->
    let addr' = fanInMaybe . fmap (match =<<) . firstIn $ addr
    in runRWS (unAddressing body) (addr', wr, addrs) s

gated :: Signal dom Bool -> FanIn dom a -> FanIn dom a
gated p sig = fanInMaybe $ mux p (firstIn sig) (pure Nothing)

tag
    :: addr'
    -> Addressing s dom dat (addr', addr) a
    -> Addressing s dom dat addr a
tag t = matchAddr $ \addr -> Just (t, addr)

matchLeft
    :: Addressing s dom dat addr1 a
    -> Addressing s dom dat (Either addr1 addr2) a
matchLeft = matchAddr $ either Just (const Nothing)

matchRight
    :: Addressing s dom dat addr2 a
    -> Addressing s dom dat (Either addr1 addr2) a
matchRight = matchAddr $ either (const Nothing) Just

override
    :: Signal dom (Maybe dat)
    -> Addressing s dom dat addr a
    -> Addressing s dom dat addr a
override sig = Addressing . censor (first $ mappend sig') . unAddressing
  where
    sig' = gated (isJust <$> sig) (fanIn sig)

from
    :: forall addr' dat addr a dom s. (Integral addr, Ord addr, Integral addr', Bounded addr')
    => addr
    -> Addressing s dom dat addr' a
    -> Addressing s dom dat addr a
from base = matchAddr $ \addr -> do
    guard $ addr >= base
    let offset = addr - base
    guard $ offset <= lim
    return $ fromIntegral offset
  where
    lim = fromIntegral (maxBound :: addr')

connect
    :: Component s addr
    -> Addressing s dom dat addr ()
connect component@(Component i) = Addressing $ do
    (addr, _, _) <- ask
    tell (mempty, AddrMap $ Map.singleton i $ unsafeCoerce addr)

firstIn :: FanIn dom a -> Signal dom (Maybe a)
firstIn = fmap getFirst . getAp . getFanIn

fanInMaybe :: Signal dom (Maybe a) -> FanIn dom a
fanInMaybe = FanIn . Ap . fmap First

fanIn :: Signal dom a -> FanIn dom a
fanIn = fanInMaybe . fmap pure
