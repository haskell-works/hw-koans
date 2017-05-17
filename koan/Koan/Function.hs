module Koan.Function where

import           Prelude          hiding ((.), ($))

import           Koan.Functor     as K
import           Koan.Applicative as K
import           Koan.Monad       as K

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = error "TODO: Implement (.)"

flip :: (a -> b -> c) -> b -> a -> c
flip = error "TODO: Implement flip"

($) :: (a -> b) -> a -> b
($) = error "TODO: Implement ($)"

instance K.Functor ((->) a) where
  fmap = error "TODO: Implement fmap for (->)"

instance K.Applicative ((->) a) where
  pure = error "TODO: Implement Applicative pure for (->)"
  (<*>) = error "TODO: Implement Applicative (<*>) for (->)"

instance K.Monad ((->) a) where
  (>>=) = error "TODO: Implement Monad (>>=) for (->)"
