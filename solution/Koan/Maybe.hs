module Koan.Maybe where

import Prelude hiding (Maybe (..))

enrolled :: Bool
enrolled = False

data Maybe a = Nothing | Just a deriving (Eq, Show)

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing  = False

isNothing :: Maybe a -> Bool
isNothing (Just _) = False
isNothing Nothing  = True

fromMaybe :: a -> Maybe a -> a
fromMaybe x Nothing  = x
fromMaybe _ (Just a) = a

orMaybe :: Maybe a -> Maybe a -> Maybe a
orMaybe (Just x) _  = Just x
orMaybe _        my = my

mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe _  Nothing = Nothing
mapMaybe f (Just x) = Just (f x)

maybe :: b -> (a -> b) -> Maybe a -> b
maybe _ f (Just a) = f a
maybe b _ Nothing  = b

catMaybes :: [Maybe a] -> [a]
catMaybes [] = []
catMaybes (mx:mxs) = case mx of
  Nothing -> catMaybes mxs
  Just x  -> x : catMaybes mxs

filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe _ Nothing = Nothing
filterMaybe p (Just x)
  | p x       = Just x
  | otherwise = Nothing

foldMaybe :: (b -> a -> b) -> b -> Maybe a -> b
foldMaybe _  acc Nothing  = acc
foldMaybe op acc (Just x) = op acc x

applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe (Just f) (Just x) = Just (f x)
applyMaybe _        _        = Nothing

bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe _ Nothing  = Nothing
bindMaybe f (Just x) = f x

instance Functor Maybe where
  fmap = mapMaybe

instance Applicative Maybe where
  pure = Just
  (<*>) = applyMaybe

instance Monad Maybe where
  (>>=) = flip bindMaybe

computeSumInDo :: Maybe Int -> Maybe Int -> Maybe Int
computeSumInDo getIntA getIntB = do
  a <- getIntA
  b <- getIntB
  return (a + b)

computeSumWithApplicative :: Maybe Int -> Maybe Int -> Maybe Int
computeSumWithApplicative getIntA getIntB = (+) <$> getIntA <*> getIntB

type Host = String
type Port = Int

data EndPoint = EndPoint
  { host :: Host
  , port :: Port
  } deriving (Eq, Show)

mkEndPoint :: Maybe Host -> Maybe Port -> Maybe EndPoint
mkEndPoint mHost mPort = EndPoint <$> mHost <*> mPort

data Protocol = Http | Ftp deriving (Eq, Show)

data Connection = Connection
  { srcEndPoint :: EndPoint
  , dstEndPoint :: EndPoint
  } deriving (Eq, Show)

mkConnection :: Maybe Host -> Maybe Port -> Maybe Host -> Maybe Port -> Maybe Connection
mkConnection mSrcHost mSrcPort mDstHost mDstPort =
  Connection <$> (EndPoint <$> mSrcHost <*> mSrcPort) <*> (EndPoint <$> mDstHost <*> mDstPort)
