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

{-
data Maybe a = Just a | Nothing deriving (Eq, Show)

just :: a -> Maybe a
just = Just

fmapInMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapInMaybe f (Just a) = Just (f a)
fmapInMaybe _ _        = Nothing

applyInMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyInMaybe (Just f) (Just a) = Just (f a)
applyInMaybe _ _               = Nothing

{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b
-}

instance Functor Maybe where
  fmap = fmapInMaybe

{-
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-}

instance Applicative Maybe where
  pure = Just
  (<*>) = applyInMaybe

add3InMaybe :: Maybe (Int -> Int)
add3InMaybe = pure (+3)

just9 :: Maybe Int
just9 = Just 9

add3To9InMaybe :: Maybe Int
add3To9InMaybe = add3InMaybe <*> just9

type Host = String
type Port = Int

data EndPoint = EndPoint
  { host :: Host
  , port :: Port
  } deriving (Eq, Show)

mkEndPoint :: Maybe Host -> Maybe Port -> Maybe EndPoint
mkEndPoint mHost mPort = EndPoint <$> mHost <*> mPort

data Protocol = Http | Ftp deriving (Eq, Show)

type Path = String

data Url = Url
  { protocol :: Protocol
  , endPoint :: EndPoint
  , path     :: Path
  } deriving (Eq, Show)

data Connection = Connection
  { srcEndPoint :: EndPoint
  , dstEndPoint :: EndPoint
  } deriving (Eq, Show)

mkConnection :: Maybe Host -> Maybe Port -> Maybe Host -> Maybe Port -> Maybe Connection
mkConnection mSrcHost mSrcPort mDstHost mDstPort =
  Connection <$> (EndPoint <$> mSrcHost <*> mSrcPort) <*> (EndPoint <$> mDstHost <*> mDstPort)

-}
