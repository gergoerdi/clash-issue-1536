{-# LANGUAGE RankNTypes #-}
module DAssoc where

import Data.Kind
import Data.Dependent.Sum as DSum
import Data.GADT.Compare
import Type.Reflection

import Clash.Prelude hiding (lookup, fold)

type DMap k f = [DSum k f]

unionWithKey :: (GCompare k) => (forall a. k a -> f a -> f a -> f a) -> DMap k f -> DMap k f -> DMap k f
unionWithKey f l r = case l of
    [] -> r
    ((k :=> v) : xs) -> unionWithKey f (insertWithKey f k v l) xs

fold :: (a -> b -> b) -> b -> [a] -> b
fold _ z []     = z
fold f z (x:xs) = fold f (f x z) xs

insertWithKey :: (GCompare k) => (k a -> f a -> f a -> f a) -> k a -> f a -> DMap k f -> DMap k f
insertWithKey f k0 v0 = go
  where
    go [] = [k0 :=> v0]
    go ((k1 :=> v1) : xs) = case gcompare k0 k1 of
        GLT -> (k0 :=> v0) : (k1 :=> v1) : xs
        GEQ -> (k0 :=> f k0 v0 v1) : xs
        GGT -> (k1 :=> v1) : go xs

lookup :: (GCompare k) => k a -> DMap k f -> Maybe (f a)
lookup k0 [] = Nothing
lookup k0 ((k :=> v) : xs) = case geq k0 k of
    Just Refl -> Just v
    Nothing -> lookup k0 xs

singleton :: k a -> f a -> DMap k f
singleton k v = [k :=> v]
