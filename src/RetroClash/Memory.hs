{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module RetroClash.Memory
    ( memoryMap, memoryMap_

    , readWrite, readWrite_
    , romFromFile, ram0
    , connect

    , override

    , from
    , matchLeft, matchRight
    , tag

    , Component(..)
    , fanInMaybe
    ) where

import Clash.Prelude
import Control.Arrow (first, second)
import Data.Maybe
import Control.Monad
import Control.Monad.RWS

import Data.Kind
import Data.Dependent.Map as DMap
import Data.GADT.Compare
import Type.Reflection

data Component s (addr :: Type) = Component (TypeRep addr) Int

instance GEq (Component s) where
    geq (Component a _) (Component b _) = geq a b

instance GCompare (Component s) where
    gcompare (Component a _) (Component b _) = gcompare a b

newtype FanIn dom a = FanIn{ getFanIn :: Signal dom `Ap` First a }
    deriving newtype (Semigroup, Monoid)

newtype AddrMap s dom = AddrMap{ addrMap :: DMap (Component s) (FanIn dom) }
    deriving newtype (Monoid)

instance Semigroup (AddrMap s dom) where
    AddrMap map1 <> AddrMap map2 = AddrMap $ unionWithKey (const mappend) map1 map2

newtype Addressing s dom dat addr a = Addressing
    { unAddressing :: RWS
          (FanIn dom addr, Signal dom (Maybe dat), AddrMap s dom)
          (FanIn dom (Maybe dat), AddrMap s dom)
          Int
          a
    }
    deriving newtype (Functor, Applicative, Monad)

{-# INLINE memoryMap #-}
memoryMap
    :: Signal dom (Maybe addr)
    -> Signal dom (Maybe dat)
    -> DMap (Component s) (FanIn dom)
    -> Addressing s dom dat addr a
    -> (Signal dom (Maybe dat), a)
memoryMap addr wr conns0 body = (join <$> firstIn read, x)
  where
    (x, (read, conns)) = evalRWS (unAddressing body) (fanInMaybe addr, wr, AddrMap conns0) 0

{-# INLINE memoryMap_ #-}
memoryMap_
    :: Signal dom (Maybe addr)
    -> Signal dom (Maybe dat)
    -> DMap (Component s) (FanIn dom)
    -> Addressing s dom dat addr ()
    -> Signal dom (Maybe dat)
memoryMap_ addr wr conns0 body = fst $ memoryMap addr wr conns0 body

{-# INLINE readWrite #-}
readWrite
    :: (HiddenClockResetEnable dom, Typeable addr')
    => (Signal dom (Maybe addr') -> Signal dom (Maybe dat) -> (Signal dom (Maybe dat), a))
    -> Addressing s dom dat addr (Component s addr', a)
readWrite mkComponent = Addressing $ do
    component <- Component typeRep <$> get <* modify succ
    (_, wr, addrs) <- ask
    let addr = firstIn . fromMaybe mempty $ DMap.lookup component (addrMap addrs)
        selected = isJust <$> addr
    let (read, x) = mkComponent addr wr
    tell (gated (delay False selected) (fanIn read), mempty)
    return (component, x)

{-# INLINE readWrite_ #-}
readWrite_
    :: (HiddenClockResetEnable dom, Typeable addr')
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

{-# INLINE matchAddr #-}
matchAddr
    :: (addr -> Maybe addr')
    -> Addressing s dom dat addr' a
    -> Addressing s dom dat addr a
matchAddr match body = Addressing $ rws $ \(addr, wr, addrs) s ->
  let addr' = fanInMaybe . fmap (match =<<) . firstIn $ addr
      (x, s', (read, components)) = runRWS (unAddressing body) (addr', wr, addrs) s
  in (x, s', (read, components))

{-# INLINE gated #-}
gated :: Signal dom Bool -> FanIn dom a -> FanIn dom a
gated p sig = fanInMaybe $ mux p (firstIn sig) (pure Nothing)

{-# INLINE tag #-}
tag
    :: addr'
    -> Addressing s dom dat (addr', addr) a
    -> Addressing s dom dat addr a
tag t = matchAddr $ \addr -> Just (t, addr)

{-# INLINE matchLeft #-}
matchLeft
    :: Addressing s dom dat addr1 a
    -> Addressing s dom dat (Either addr1 addr2) a
matchLeft = matchAddr $ either Just (const Nothing)

{-# INLINE matchRight #-}
matchRight
    :: Addressing s dom dat addr2 a
    -> Addressing s dom dat (Either addr1 addr2) a
matchRight = matchAddr $ either (const Nothing) Just

{-# INLINE override #-}
override
    :: Signal dom (Maybe dat)
    -> Addressing s dom dat addr a
    -> Addressing s dom dat addr a
override sig = Addressing . censor (first $ mappend sig') . unAddressing
  where
    sig' = gated (isJust <$> sig) (fanIn sig)

{-# INLINE from #-}
from
    :: forall addr' s dom dat addr a. (Integral addr, Ord addr, Integral addr', Bounded addr')
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

{-# INLINE connect #-}
connect
    :: Component s addr
    -> Addressing s dom dat addr ()
connect component@(Component _ i) = Addressing $ do
    (addr, _, _) <- ask
    tell (mempty, AddrMap $ DMap.singleton component addr)

{-# INLINE firstIn #-}
firstIn :: FanIn dom a -> Signal dom (Maybe a)
firstIn = fmap getFirst . getAp . getFanIn

{-# INLINE fanInMaybe #-}
fanInMaybe :: Signal dom (Maybe a) -> FanIn dom a
fanInMaybe = FanIn . Ap . fmap First

{-# INLINE fanIn #-}
fanIn :: Signal dom a -> FanIn dom a
fanIn = fanInMaybe . fmap pure
