module Koan.Monad where

import Prelude hiding (Monad, fmap, return, (<$), (<$>), (>>), (>>=))

enrolled :: Bool
enrolled = False

class Applicative m => Monad m  where
    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    (>>) ma mb = ma >>= const mb

    return :: a -> m a
    return = pure

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) ma f = f >>= ma

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> a -> m c
(>=>) f g a = f a >>= g
