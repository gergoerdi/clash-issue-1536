{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Board where

import Clash.Prelude hiding (Signal)
import Data.Maybe
import Control.Monad
import Control.Monad.RWS
import Assoc as Map

import Control.Monad.Identity

type Signal (dom :: Domain) = Identity

type Key = Int

newtype Component s addr = Component Key
    deriving newtype (Eq, Ord)

newtype FanIn dom a = FanIn{ getFanIn :: Signal dom `Ap` First a }
    deriving newtype (Semigroup, Monoid)

newtype AddrMap s dom = AddrMap{ addrMap :: Map Int (FanIn dom (Index 0x0400)) }
    deriving newtype (Monoid)

instance Semigroup (AddrMap s dom) where
    AddrMap map1 <> AddrMap map2 = AddrMap $ unionWithKey (const mappend) map1 map2

newtype Addressing s dom dat addr a = Addressing
    { unAddressing :: RWS
          (FanIn dom addr, AddrMap s dom)
          (FanIn dom (Maybe dat), AddrMap s dom)
          Key
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
    :: (Signal dom (Maybe (Index 1024)) -> Signal dom (Maybe dat))
    -> Addressing s dom dat addr (Component s (Index 1024))
readWrite_ mkComponent = Addressing $ do
    component@(Component i) <- Component <$> get <* modify succ
    (_, addrs) <- ask
    let addr = firstIn . fromMaybe mempty $ Map.lookup i (addrMap addrs)
        read = mkComponent addr
    tell (fanIn read, mempty)
    return component

ram0
    :: (1 <= n)
    => SNat n
    -> Addressing s dom (Index 1024) addr (Component s (Index 1024))
ram0 size@SNat = readWrite_ $ \addr ->
    Just <$> negate . fromMaybe 2 <$> addr

connect
    :: Component s (Index 1024)
    -> Addressing s dom dat (Index 1024) ()
connect component@(Component i) = Addressing $ do
    (addr, _) <- ask
    tell (mempty, AddrMap $ Map.singleton i $ addr)

firstIn :: FanIn dom a -> Signal dom (Maybe a)
firstIn = fmap getFirst . getAp . getFanIn

fanInMaybe :: Signal dom (Maybe a) -> FanIn dom a
fanInMaybe = FanIn . Ap . fmap First

fanIn :: Signal dom a -> FanIn dom a
fanIn = fanInMaybe . fmap pure

matchAddr
    :: (addr -> Maybe addr')
    -> Addressing s dom dat addr' a
    -> Addressing s dom dat addr a
matchAddr match body = Addressing $ rws $ \(addr, addrs) s ->
  let addr' = fanInMaybe . fmap (match =<<) . firstIn $ addr
  in runRWS (unAddressing body) (addr', addrs) s

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

topEntity
    :: Signal System (Maybe (Index 0x0400))
    -> (Signal System (Maybe (Index 0x0400)), ())
topEntity addr = memoryMap addr $ do
    ram <- ram0 (SNat @0x0400)
    from 0x0000 $ connect ram
    from 0x0400 $ connect ram
