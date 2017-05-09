module Koan.Maybe where

import           Prelude hiding (Maybe (..))

enrolled :: Bool
enrolled = False

data Maybe a = Nothing | Just a deriving (Eq, Show)

orElse :: Maybe a -> a -> a
orElse Nothing x  = x
orElse (Just a) _ = a

orMaybe :: Maybe a -> Maybe a -> Maybe a
orMaybe (Just x) _  = Just x
orMaybe _        my = my

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _  Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

concatMaybes :: [Maybe a] -> [a]
concatMaybes [] = []
concatMaybes (mx:mxs) = case mx of
  Nothing -> concatMaybes mxs
  Just x  -> x : concatMaybes mxs

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe _ Nothing = Nothing
filterMaybe p (Just x)
  | p x       = Just x
  | otherwise = Nothing

foldMaybe :: (b -> a -> b) -> b -> Maybe a -> b
foldMaybe _  acc Nothing  = acc
foldMaybe op acc (Just x) = op acc x

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe Nothing  _        = Nothing
applyMaybe _        Nothing  = Nothing
applyMaybe (Just f) (Just x) = Just (f x)

bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe _ Nothing  = Nothing
bindMaybe f (Just x) = f x
