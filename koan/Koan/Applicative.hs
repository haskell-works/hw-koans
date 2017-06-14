module Koan.Applicative where

import Prelude hiding (Applicative (..), Maybe (..))

{-
## Pre-requisites
* Functor
-}

enrolled :: Bool
enrolled = False

infixl 4 <*>, <*, *>

class Functor f => Applicative f where
  pure  :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b

  (*>) :: f a -> f b -> f b
  (*>) = error "TODO: implement (*>)"

  (<*) :: f a -> f b -> f a
  (<*) = error "TODO: implement (<*)"
