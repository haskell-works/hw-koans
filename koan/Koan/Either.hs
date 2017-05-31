module Koan.Either where

import           Prelude          hiding (Either (..), isLeft, isRight, lefts, rights, either)
import           Data.Bifunctor

enrolled :: Bool
enrolled = False

data Either a b = Left a | Right b

isLeft :: Either a b -> Bool
isLeft = error "TODO: Implement isLeft"

isRight :: Either a b -> Bool
isRight = error "TODO: Implement isRight"

lefts :: [Either a b] -> [a]
lefts = error "TODO: Implement lefts"

rights :: [Either a b] -> [b]
rights = error "TODO: Implement rights"

either :: (a -> c) -> (b -> c) -> Either a b -> c
either = error "TODO: Implement either"

-- If you want a challenge, try to implement this using `foldr`
partition :: [Either a b] -> ([a], [b])
partition = error "TODO: Implement partition"

mapEither :: (b -> c) -> Either a b -> Either a c
mapEither = error "TODO: Implement mapEither"

bimapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
bimapEither = error "TODO: Implement bimapEither"

applyEither :: Either a (b -> c) -> Either a b -> Either a c
applyEither = error "TODO: Implement applyEither"

bindEither :: (b -> Either a c) -> Either a b -> Either a c
bindEither = error "TODO: Implement bindEither"

instance Functor (Either a) where
  fmap = error "TODO: implement fmap for Either a"

instance Bifunctor Either where
  bimap = error "TODO: implement bimap for Either"

instance Applicative (Either a) where
  pure = error "TODO: Implement Applicative pure for Either a"
  (<*>) = error "TODO: Implement Applicative (<*>) for Either a"

instance Monad (Either a) where
  (>>=) = error "TODO: Implement Monad (>>=) for Either a"
