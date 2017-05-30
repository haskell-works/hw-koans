module Koan.Alternative where

import Koan.Applicative as K

import Prelude hiding (Applicative (..), pure)

enrolled :: Bool
enrolled = False

infixl 3 <|>

class Applicative f => Alternative f where
  empty :: f a

  (<|>) :: f a -> f a -> f a

  some :: f a -> f [a]
  some = error "TODO: implement some"

  many :: f a -> f [a]
  many = error "TODO: implement many"
