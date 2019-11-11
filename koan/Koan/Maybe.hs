module Koan.Maybe where

import Prelude hiding (Maybe (..))

enrolled :: Bool
enrolled = False

-- Define a "Maybe" data type.
-- It should represent the concept of there being zero or exactly one instance
-- of a piece of data.
-- In other words, it should represent whether a piece of data is exists or not.

data Maybe a = Nothing | Just a deriving (Eq, Show)

-- This should return the contents of the 'Maybe' if it exists, otherwise
-- it should return the provided value.
orElse :: Maybe a -> a -> a
orElse = error "TODO: Implement orElse"

-- This should return the first Maybe if there's something in it.
-- If not, it should return the second Maybe (regardless of whether its empty
-- or not).
orMaybe :: Maybe a -> Maybe a -> Maybe a
orMaybe = error "TODO: Implement orMaybe"

-- This should apply a function to the contents of a Maybe, if it exists.
mapMaybe :: (a -> b) -> Maybe a -> Maybe b
mapMaybe = error "TODO: Implement mapMaybe"

-- Concatenate all the Maybes in the input list, so that you end up with only
-- the ones that have something in them.
concatMaybes :: [Maybe a] -> [a]
concatMaybes = error "TODO: Implement concatMaybes"

-- Filter for Maybes.  Think of Maybes as a collection of at most one element.
-- This function should remove all elements that fail the predicate (a -> Bool)
filterMaybe :: (a -> Bool) -> Maybe a -> Maybe a
filterMaybe = error "TODO: Implement filterMayber"

-- Fold for Maybes.
foldMaybe :: (b -> a -> b) -> b -> Maybe a -> b
foldMaybe = error "TODO: Implement foldMaybe"

-- Similar to the map function above, but now the function to apply is also in
-- a Maybe.
applyMaybe :: Maybe (a -> b) -> Maybe a -> Maybe b
applyMaybe = error "TODO: Implement applyMaybe"

-- Similar to the above map function, but now we must deal with
-- the input function also returning a Maybe.
bindMaybe :: (a -> Maybe b) -> Maybe a -> Maybe b
bindMaybe = error "TODO: Implement bindMaybe"

instance Functor Maybe where
  fmap = error "TODO: Implement fmap for (Maybe a)"

instance Applicative Maybe where
  pure = error "TODO: Implement Applicative pure for Maybe"
  (<*>) = error "TODO: Implement Applicative (<*>) for Maybe"

instance Monad Maybe where
  (>>=) = error "TODO: Implement Monad (>>=) for Maybe"

computeSumInDo :: Maybe Int -> Maybe Int -> Maybe Int
computeSumInDo getIntA getIntB = do
  _ <- getIntA
  _ <- getIntB
  return (error "TODO: Implement rest of computeSum")

computeSumWithApplicative :: Maybe Int -> Maybe Int -> Maybe Int
computeSumWithApplicative = error "TODO: Implement computeSumWithApplicative with (<$>) and (<*>) instead of do notation"

type Host = String
type Port = Int

data EndPoint = EndPoint
  { host :: Host
  , port :: Port
  } deriving (Eq, Show)

mkEndPoint :: Maybe Host -> Maybe Int -> Maybe EndPoint
mkEndPoint = error "TODO: Implement mkEndPoint using (<$>) and (<*>)"

data Connection = Connection
  { srcEndPoint :: EndPoint
  , dstEndPoint :: EndPoint
  } deriving (Eq, Show)

mkConnection :: Maybe Host -> Maybe Port -> Maybe Host -> Maybe Port -> Maybe Connection
mkConnection srcHost srcPort dstHost dstPort = error "TODO: Implement mkConnection using (<$>) and <*>"
