module Koan.Applicative where

import           Prelude hiding (Maybe (..))

enrolled :: Bool
enrolled = True

class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

  (*>) :: f a -> f b -> f b
  a *> b = (id <$ a) Koan.Applicative.<*> b

  (<*) :: f a -> f b -> f a
  a <* b = fmap const a Koan.Applicative.<*> b
