{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
module Board where

import Clash.Prelude
import Data.Maybe
import Control.Monad
import Control.Monad.RWS
import Assoc as Map

type Key = Int

newtype Component s addr = Component Key
    deriving newtype (Eq, Ord)

newtype FanIn a = FanIn{ getFanIn :: First a }
    deriving newtype (Semigroup, Monoid)

newtype AddrMap s = AddrMap{ addrMap :: Map Int (FanIn (Index 0x0400)) }
    deriving newtype (Monoid)

instance Semigroup (AddrMap s) where
    AddrMap map1 <> AddrMap map2 = AddrMap $ unionWithKey (const mappend) map1 map2

newtype Addressing s dat addr a = Addressing
    { unAddressing :: RWS
          (FanIn addr, AddrMap s)
          (FanIn (Maybe dat), AddrMap s)
          Key
          a
    }
    deriving newtype (Functor, Applicative, Monad)

memoryMap
    :: Maybe addr
    -> (forall s. Addressing s dat addr a)
    -> (Maybe dat, a)
memoryMap addr body = (join (firstIn read), x)
  where
    (x, (read, conns)) = evalRWS (unAddressing body) (fanInMaybe addr, conns) 0

readWrite_
    :: ((Maybe (Index 1024)) -> (Maybe dat))
    -> Addressing s dat addr (Component s (Index 1024))
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
    -> Addressing s (Index 1024) addr (Component s (Index 1024))
ram0 size@SNat = readWrite_ $ \addr ->
    Just $ negate (fromMaybe 2 addr)

connect
    :: Component s (Index 1024)
    -> Addressing s dat (Index 1024) ()
connect component@(Component i) = Addressing $ do
    (addr, _) <- ask
    tell (mempty, AddrMap $ Map.singleton i $ addr)

firstIn :: FanIn a -> Maybe a
firstIn = getFirst . getFanIn

fanInMaybe :: Maybe a -> FanIn a
fanInMaybe = FanIn . First

fanIn :: a -> FanIn a
fanIn = fanInMaybe . pure

matchAddr
    :: (addr -> Maybe addr')
    -> Addressing s dat addr' a
    -> Addressing s dat addr a
matchAddr match body = Addressing $ rws $ \(addr, addrs) s ->
  let addr' = fanInMaybe . (match =<<) . firstIn $ addr
  in runRWS (unAddressing body) (addr', addrs) s

from
    :: forall addr' dat addr a s. (Integral addr, Ord addr, Integral addr', Bounded addr')
    => addr
    -> Addressing s dat addr' a
    -> Addressing s dat addr a
from base = matchAddr $ \addr -> do
    guard $ addr >= base
    let offset = addr - base
    guard $ offset <= lim
    return $ fromIntegral offset
  where
    lim = fromIntegral (maxBound :: addr')

topEntity
    :: Maybe (Index 0x0400)
    -> ((Maybe (Index 0x0400)), ())
topEntity addr = memoryMap addr $ do
    ram <- ram0 (SNat @0x0400)
    from 0x0000 $ connect ram
    from 0x0400 $ connect ram
