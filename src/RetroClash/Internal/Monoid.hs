module RetroClash.Internal.Monoid where

import Prelude

newtype First a = First{ getFirst :: Maybe a}

instance Semigroup (First a) where
    x@(First (Just _)) <> _ = x
    First Nothing <> y = y

instance Monoid (First a) where
    mempty = First Nothing

newtype Ap f a = Ap{ getAp :: f a }

instance (Applicative f, Semigroup a) => Semigroup (Ap f a) where
    Ap fx <> Ap fy = Ap $ (<>) <$> fx <*> fy

instance (Applicative f, Monoid a) => Monoid (Ap f a) where
    mempty = Ap $ pure mempty
