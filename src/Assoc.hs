{-# LANGUAGE DerivingStrategies, GeneralizedNewtypeDeriving #-}
module Assoc where

import Clash.Prelude hiding (lookup, fold)

type Map k a = [(k, a)]

unionWithKey :: Ord k => (k -> a -> a -> a) -> Map k a -> Map k a -> Map k a
unionWithKey f l r = fold (insertWithKey f) r l

fold :: (a -> b -> b) -> b -> [a] -> b
fold _ z []     = z
fold f z (x:xs) = fold f (f x z) xs

insertWithKey :: Ord k => (k -> a -> a -> a) -> (k,a) -> Map k a -> Map k a
insertWithKey _ k [] = [k]
insertWithKey f (k,a) ((k1,b):xs) = case compare k k1 of
  LT -> (k,a):(k1,b):xs
  EQ -> (k,f k a b) : xs
  GT -> (k1,b):insertWithKey f (k,a) xs

lookup :: Eq k => k -> Map k a -> Maybe a
lookup _ [] = Nothing
lookup k ((k1,a):xs) = if k == k1 then Just a else lookup k xs

singleton :: k -> a -> Map k a
singleton k a = [(k, a)]
