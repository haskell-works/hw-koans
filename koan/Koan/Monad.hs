module Koan.Monad where

import           Prelude hiding (Monad, fmap, (<$), (<$>))

enrolled :: Bool
enrolled = False

class Monad m  where
    (>>=) :: m a -> (a -> m b) -> m b

    (>>) :: m a -> m b -> m b
    (>>) = error "TODO: Implement (>>)"

    return :: a -> m a
    return = error "TODO: Implement return"

(=<<) :: Monad m => (a -> m b) -> m a -> m b
(=<<) = error "TODO: Implement (==<)"

(>=>) :: Monad m => (a -> m b) -> (b -> m c) -> (a -> m c)
(>=>) = error "TODO: Implement (>=>)"
