module Koan.Applicative where

import           Prelude hiding (Maybe (..))

enrolled :: Bool
enrolled = False

data Maybe a = Just a | Nothing deriving (Eq, Show)

just :: a -> Maybe a
just = error "TODO: Implement just"

fmapInMaybe :: (a -> b) -> Maybe a -> Maybe b
fmapInMaybe  = error "TODO: Implement fmapInMaybe"

applyInMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyInMaybe = error "TODO: Implement applyInMaybe"

{-
class Functor f where
  fmap :: (a -> b) -> f a -> f b
-}

instance Functor Maybe where
  fmap = error "TODO: Implement fmap"

{-
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
-}

instance Applicative Maybe where
  pure = error "TODO: Implement pure"
  (<*>) = error "TODO: Implement (<*>)"

add3InMaybe :: Maybe (Int -> Int)
add3InMaybe = pure (+3)

just9 :: Maybe Int
just9 = Just 9

add3To9InMaybe :: Maybe Int
add3To9InMaybe = error "TODO: Implement add3To9InMaybe using add3InMaybe, just9 and (<*>)"

type Host = String
type Port = Int

data EndPoint = EndPoint
  { host :: Host
  , port :: Port
  } deriving (Eq, Show)

mkEndPoint :: Maybe Host -> Maybe Int -> Maybe EndPoint
mkEndPoint = error "TODO: Implement mkEndPoint using (<$>) and (<*>)"

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
mkConnection srcHost srcPort dstHost dstPort = error "TODO: Implement mkConnection using (<$>) and <*>"
