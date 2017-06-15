{-# LANGUAGE InstanceSigs #-}
module Koan.Reader where

import Control.Applicative
import Data.Char
import Data.List
import Data.Semigroup

import Prelude

enrolled :: Bool
enrolled = False

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap f ra = Reader (f . runReader ra)

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure a = Reader (const a)

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  rf <*> ra = Reader $ \r ->
    let f = runReader rf r
        a = runReader ra r
    in f a

instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  ra >>= f = Reader $ \r -> runReader (f (runReader ra r)) r

ask :: Reader r r
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader

-------------------------------------------------------------------------------
type Path = String
newtype Url = Url String deriving (Show, Eq)
newtype ShellCommand = ShellCommand String deriving (Show, Eq)
data Options = Options
  { host :: String
  , port :: Int
  } deriving (Show, Eq)

hostUrl :: Reader Options Url
hostUrl = do
  h <- asks host
  p <- asks (show . port)
  return . Url $ "https://" <> h <> ":" <> p

curlCmd :: Path -> Reader Options ShellCommand
curlCmd path = do
  Url url <- hostUrl
  return . ShellCommand $ "curl " <> url <> "/" <> path
