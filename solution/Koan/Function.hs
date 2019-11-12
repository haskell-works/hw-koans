module Koan.Function where

import Koan.Applicative as K
import Koan.Functor     as K
import Koan.Monad       as K
import Prelude          hiding (($), (.))

enrolled :: Bool
enrolled = True

(.) :: (b -> c) -> (a -> b) -> a -> c
(.) f g x = f (g x)

flip :: (a -> b -> c) -> b -> a -> c
flip f b a = f a b

-- Hint: You're already implemented this.
mapFunction :: (a -> b) -> (r -> a) -> r -> b
mapFunction = (.)

applyFunction :: (r -> a -> b) -> (r -> a) -> r -> b
applyFunction f g x = f x (g x)

bindFunction :: (a -> r -> b) -> (r -> a) -> r -> b
bindFunction f k r = f (k r) r

instance K.Functor ((->) r) where
  fmap = mapFunction

instance K.Applicative ((->) r) where
  pure = const
  (<*>) = applyFunction

instance K.Monad ((->) r) where
  (>>=) ma f = bindFunction f ma
