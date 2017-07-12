module Koan.GeoFeedParser
where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as BS
import           Data.Char                        (digitToInt)
import           Data.Semigroup                   ((<>))

-- Parsing library, reference doc: http://hackage.haskell.org/package/attoparsec-0.13.1.0/docs/Data-Attoparsec-ByteString-Char8.html

data IPv4 = IPv4 Int Int Int Int deriving (Eq, Ord, Show)
newtype CountryCode = CountryCode String deriving (Eq, Ord, Show)

-- A feed row representation. It contains 3 fields:
-- "startIp" and "endIp" are required, and "cc" is optional
data FeedRow = FeedRow
  { startIp :: IPv4
  , lastIp  :: IPv4
  , cc      :: Maybe CountryCode
  } deriving (Eq, Ord, Show)

-- Assuming that characters in a list are valid digits
digitsToInt :: [Char] -> Int
digitsToInt as = foldl (\b a -> b * 10 + digitToInt a) 0 as
-- alternatively this, but it is much less cool ;)
-- digitsToInt = read

int :: Parser Int
int = digitsToInt <$> many1 digit

octet :: Parser Int
octet = int >>= \x ->
  if x >= 0 && x <= 255 then return x
  else fail $ "IPv4 octet must be in range [0..255], but was " <> show x

dot :: Parser Char
dot = char '.'

separator :: Parser Char
separator = char '|'

ipv4 :: Parser IPv4
ipv4 = IPv4
  <$> int <* dot
  <*> int <* dot
  <*> int <* dot
  <*> int

-- {- Alternatively we can use "do" syntax -}
-- ipv4 = do
--   a <- int
--   dot
--   b <- int
--   dot
--   c <- int
--   dot
--   d <- int
--   return $ IPv4 a b c d

countryCode :: Parser CountryCode
countryCode = CountryCode <$> many1 letter_ascii

row :: Parser FeedRow
row = FeedRow
      <$> ipv4 <* separator
      <*> ipv4 <* separator
      <*> optional countryCode

-- {- Alternatively we can use "do" syntax -}
-- row = do
--   sip <- ipv4
--   separator
--   eip <- ipv4
--   separator
--   cc <- countryCode
--   return $ FeedRow sip eip (Just cc)

feedFile :: FilePath
feedFile = "../data/GeoFeed.dsv"

parseFeed :: IO ()
parseFeed = do
  bs <- BS.readFile feedFile
  let results = parseOnly row <$> BS.lines bs
  print results
