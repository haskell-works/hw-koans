{-# LANGUAGE InstanceSigs #-}
module Koan.Reader where

import Control.Applicative
import Data.Char
import Data.List
import Data.Semigroup

import Prelude

enrolled :: Bool
enrolled = False

-- | Reader keeps an "environment" of type "r", and "a" is a result type
--
-- runReader :: Reader r a -> r -> a
-- Notice that "runReader" doesn't do anything with "r"
newtype Reader r a = Reader { runReader :: r -> a }

-- Hint: "runReader" may help getting access to the value
instance Functor (Reader r) where
  fmap :: (a -> b) -> Reader r a -> Reader r b
  fmap = error "TODO: Implement fmap"

instance Applicative (Reader r) where
  pure :: a -> Reader r a
  pure = error "TODO: Implement pure"

  (<*>) :: Reader r (a -> b) -> Reader r a -> Reader r b
  (<*>) = error "TODO: Implement (<*>)"

-- Hint: use "runReader" function when necessary
instance Monad (Reader r) where
  (>>=) :: Reader r a -> (a -> Reader r b) -> Reader r b
  (>>=) = error "TODO: Implement (>>=)"

-- | Returns an "environment".
ask :: Reader r r
ask = error "TODO: Implement ask"

-- | Applies a function to an "environment" and returns the result
asks :: (r -> a) -> Reader r a
asks = error "TODO: Implement asks"

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

-- Using "hostUrl" construct a curl command
-- The command should look like:
--   curl https://<host>:<port>/path
curlCmd :: Path -> Reader Options ShellCommand
curlCmd = error "TODO: implement curlCmd"
