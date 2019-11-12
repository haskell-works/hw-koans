module Koan.Function where

import Koan.Applicative as K
import Koan.Functor     as K
import Koan.Monad       as K
import Prelude          hiding (($), (.))

enrolled :: Bool
enrolled = False

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) = error "TODO: Implement (.)"

flip :: (a -> b -> c) -> b -> a -> c
flip = error "TODO: Implement flip"

-- Hint: You're already implemented this.
mapFunction :: (a -> b) -> (r -> a) -> r -> b
mapFunction = error "TODO: Implement mapFunction"

applyFunction :: (r -> a -> b) -> (r -> a) -> r -> b
applyFunction = error "TODO: implement applyFunction"

bindFunction :: (a -> r -> b) -> (r -> a) -> r -> b
bindFunction = error "TODO: implement bindFunction"

instance K.Functor ((->) r) where
  fmap = error "TODO: Implement fmap for (->)"

instance K.Applicative ((->) r) where
  pure = error "TODO: Implement Applicative pure for (->)"
  (<*>) = error "TODO: Implement Applicative (<*>) for (->)"

instance K.Monad ((->) r) where
  (>>=) = error "TODO: Implement Monad (>>=) for (->)"
