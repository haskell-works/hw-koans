module Koan.Alternative where

import Koan.Applicative as K

import Prelude hiding (Applicative (..), pure)

enrolled :: Bool
enrolled = True

infixl 3 <|>

class Applicative f => Alternative f where
  empty :: f a

  (<|>) :: f a -> f a -> f a

  some :: f a -> f [a]
  some v = some_v
    where many_v = some_v <|> pure []
          some_v = fmap (:) v <*> many_v

  many :: f a -> f [a]
  many v = many_v
    where many_v = some_v <|> pure []
          some_v = fmap (:) v <*> many_v
