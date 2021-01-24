{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module RetroClash.Memory where

import RetroClash.Port
import Clash.Prelude
import Control.Arrow (first, second)
import Data.Maybe
import Control.Monad
import RetroClash.Internal.RWS
import RetroClash.Internal.Monoid

import Unsafe.Coerce
import RetroClash.Internal.Assoc as Map

type Key = Int

newtype Component s addr = Component Key
    deriving newtype (Eq, Ord)

newtype FanIn dom a = FanIn{ getFanIn :: Signal dom `Ap` First a }
    deriving newtype (Semigroup, Monoid)

newtype AddrMap s dom = AddrMap{ addrMap :: Map Key (FanIn dom ()) }
    deriving newtype (Monoid)

instance Semigroup (AddrMap s dom) where
    AddrMap map1 <> AddrMap map2 = AddrMap $ unionWithKey (const $ flip (<>)) map1 map2

newtype ReadMap s dom dat = ReadMap{ readMap :: Map Key (FanIn dom (Maybe dat)) }
    deriving newtype (Monoid)

instance Semigroup (ReadMap s dom dat) where
    ReadMap map1 <> ReadMap map2 = ReadMap $ Map.unionWithKey (const mappend) map1 map2

newtype Addressing s dom dat addr a = Addressing
    { unAddressing :: RWS
          (FanIn dom addr, Signal dom (Maybe dat), ReadMap s dom dat, AddrMap s dom)
          (FanIn dom (Maybe dat), ReadMap s dom dat, AddrMap s dom)
          Key
          a
    }
    deriving newtype (Functor, Applicative, Monad)

{-# INLINE memoryMap #-}
memoryMap
    :: Signal dom (Maybe addr)
    -> Signal dom (Maybe dat)
    -> (forall s. Addressing s dom dat addr a)
    -> (Signal dom (Maybe dat), a)
memoryMap addr wr body = (join <$> firstIn read, x)
  where
    (x, (read, reads, conns)) = evalRWS (unAddressing body) (fanInMaybe addr, wr, reads, conns) 0

{-# INLINE memoryMap_ #-}
memoryMap_
    :: Signal dom (Maybe addr)
    -> Signal dom (Maybe dat)
    -> (forall s. Addressing s dom dat addr ())
    -> Signal dom (Maybe dat)
memoryMap_ addr wr body = fst $ memoryMap addr wr body

{-# INLINE conduit #-}
conduit
    :: (HiddenClockResetEnable dom)
    => Signal dom (Maybe dat)
    -> Addressing s dom dat addr (Component s addr', Signal dom (Maybe addr'), Signal dom (Maybe dat))
conduit read = do
    (component, (addr, wr)) <- readWrite $ \addr wr -> (read, (addr, wr))
    return (component, addr, wr)

{-# INLINE readWrite #-}
readWrite
    :: forall addr' addr dat a dom s. (HiddenClockResetEnable dom)
    => (Signal dom (Maybe addr') -> Signal dom (Maybe dat) -> (Signal dom (Maybe dat), a))
    -> Addressing s dom dat addr (Component s addr', a)
readWrite mkComponent = Addressing $ do
    component@(Component i) <- Component <$> get <* modify succ
    (_, wr, _, addrs) <- ask
    let addr = firstIn . fromMaybe (error "readWrite") $ Map.lookup i (addrMap addrs)
        (read, x) = mkComponent (unsafeCoerce addr) wr
    tell (mempty, ReadMap $ Map.singleton i (fanIn read), mempty)
    return (component, x)

{-# INLINE readWrite_ #-}
readWrite_
    :: forall addr' addr dat dom s. (HiddenClockResetEnable dom)
    => (Signal dom (Maybe addr') -> Signal dom (Maybe dat) -> Signal dom (Maybe dat))
    -> Addressing s dom dat addr (Component s addr')
readWrite_ mkComponent = fmap fst $ readWrite $ \addr wr -> (mkComponent addr wr, ())

{-# INLINE romFromFile #-}
romFromFile
    :: (HiddenClockResetEnable dom, 1 <= n, BitPack dat)
    => SNat n
    -> FilePath
    -> Addressing s dom dat addr (Component s (Index n))
romFromFile size@SNat fileName = readWrite_ $ \addr _wr ->
    fmap (Just . unpack) $ romFilePow2 fileName (maybe 0 bitCoerce <$> addr)

{-# INLINE ram0 #-}
ram0
    :: (HiddenClockResetEnable dom, 1 <= n, NFDataX dat, Num dat)
    => SNat n
    -> Addressing s dom dat addr (Component s (Index n))
ram0 size@SNat = readWrite_ $ \addr wr ->
    fmap Just $ blockRam1 ClearOnReset size 0 (fromMaybe 0 <$> addr) (liftA2 (,) <$> addr <*> wr)

type Port dom addr dat a = Signal dom (Maybe (PortCommand addr dat)) -> (Signal dom (Maybe dat), a)
type Port_ dom addr dat = Signal dom (Maybe (PortCommand addr dat)) -> Signal dom (Maybe dat)

{-# INLINE port #-}
port
    :: (HiddenClockResetEnable dom, NFDataX dat)
    => Port dom addr' dat a
    -> Addressing s dom dat addr (Component s addr', a)
port mkPort = readWrite $ \addr wr ->
    let (read, x) = mkPort $ portFromAddr addr wr
    in (delay Nothing read, x)

port_
    :: (HiddenClockResetEnable dom, NFDataX dat)
    => Port_ dom addr' dat
    -> Addressing s dom dat addr (Component s addr')
port_ mkPort = readWrite_ $ \addr wr ->
    let read = mkPort $ portFromAddr addr wr
    in (delay Nothing read)

{-# INLINE matchAddr #-}
matchAddr
    :: (addr -> Maybe addr')
    -> Addressing s dom dat addr' a
    -> Addressing s dom dat addr a
matchAddr match body = Addressing $ rws $ \(addr, wr, reads, addrs) s ->
    let addr' = fanInMaybe . fmap (match =<<) . firstIn $ addr
        selected = isJust <$> firstIn addr'
    in runRWS (unAddressing body) (addr', wr, reads, addrs) s

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

{-# INLINE override #-}
override
    :: Signal dom (Maybe dat)
    -> Addressing s dom dat addr ()
override sig = Addressing $ do
    (addr, _, _, _) <- ask
    let selected = isJust <$> firstIn addr
        sig' = gated (selected .&&. isJust <$> sig) (fanIn sig)
    tell (sig', mempty, mempty)

from
    :: forall addr' dat addr a dom s. (Integral addr, Ord addr, Integral addr', Bounded addr')
    => addr
    -> Addressing s dom dat addr' a
    -> Addressing s dom dat addr a
from base = matchAddr $ from_ base (maxBound :: addr')

from_
    :: forall addr' addr. (Integral addr, Ord addr, Integral addr', Bounded addr')
    => addr
    -> addr'
    -> addr -> Maybe addr'
from_ base lim addr = do
    guard $ addr >= base
    let offset = addr - base
    guard $ offset <= fromIntegral lim
    return $ fromIntegral offset

{-# INLINE connect #-}
connect
    :: (HiddenClockResetEnable dom)
    => Component s addr
    -> Addressing s dom dat addr ()
connect component@(Component i) = Addressing $ do
    (addr, _, reads, _) <- ask
    let read = fromMaybe (error "connect") $ Map.lookup i (readMap reads)
        selected = isJust <$> firstIn addr
    tell (gated (delay False selected) read, mempty, AddrMap $ Map.singleton i $ unsafeCoerce addr)

firstIn :: FanIn dom a -> Signal dom (Maybe a)
firstIn = fmap getFirst . getAp . getFanIn

fanInMaybe :: Signal dom (Maybe a) -> FanIn dom a
fanInMaybe = FanIn . Ap . fmap First

fanIn :: Signal dom a -> FanIn dom a
fanIn = fanInMaybe . fmap pure
