module Koan.Either where

import Data.Bifunctor
import Prelude        hiding (Either (..), either, isLeft, isRight, lefts, rights)

enrolled :: Bool
enrolled = True

data Either a b = Left a | Right b

isLeft :: Either a b -> Bool
isLeft (Left _) = True
isLeft _        = False

isRight :: Either a b -> Bool
isRight (Right _) = True
isRight _         = False

lefts :: [Either a b] -> [a]
lefts es = [a | Left a <- es]

rights :: [Either a b] -> [b]
rights es = [b | Right b <- es]

either :: (a -> c) -> (b -> c) -> Either a b -> c
either f _ (Left a)  = f a
either _ g (Right b) = g b

partition :: [Either a b] -> ([a], [b])
partition es = (lefts es, rights es)

mapEither :: (b -> c) -> Either a b -> Either a c
mapEither _ (Left a)  = Left a
mapEither f (Right b) = Right (f b)

bimapEither :: (a -> c) -> (b -> d) -> Either a b -> Either c d
bimapEither f _ (Left a)  = Left (f a)
bimapEither _ g (Right b) = Right (g b)

applyEither :: Either a (b -> c) -> Either a b -> Either a c
applyEither (Left e) _  = Left e
applyEither (Right f) r = mapEither f r

bindEither :: (b -> Either a c) -> Either a b -> Either a c
bindEither _ (Left l)  = Left l
bindEither f (Right r) = f r

instance Functor (Either a) where
  fmap = error "TODO: implement fmap for Either a"

instance Bifunctor Either where
  bimap = error "TODO: implement bimap for Either"

instance Applicative (Either a) where
  pure = error "TODO: Implement Applicative pure for Either a"
  (<*>) = error "TODO: Implement Applicative (<*>) for Either a"

instance Monad (Either a) where
  (>>=) = error "TODO: Implement Monad (>>=) for Either a"
