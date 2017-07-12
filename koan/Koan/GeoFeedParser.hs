module Koan.GeoFeedParser
where

import           Control.Applicative
import           Data.Attoparsec.ByteString.Char8
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Char8            as BS
import           Data.Char                        (digitToInt)
import           Data.Semigroup                   ((<>))

enrolled :: Bool
enrolled = False

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

-- We use '|' as separating char
separator :: Parser Char
separator = error "TODO: separator :: Parser Char"

-- "." as a dot within an IP address
dot :: Parser Char
dot = error "TODO: dot :: Parser Char"

-- Assuming that characters in a list are valid digits
digitsToInt :: [Char] -> Int
digitsToInt = error "TODO: digitsToInt :: [Char] -> Int"

-- An Int parser should parse a number of digits and convert them into an Int
int :: Parser Int
int = error "TODO: int :: Parser Int"

-- A valid IPv4 octet is an Int that is in range [0..255]
-- Consider using "int" parser (a container with an Int inside it)
-- and make a decision on that value inside the container
octet :: Parser Int
octet = error "TODO: octet :: Parser Int"

-- Parse IPv4.
-- Either applicative style or do-notation may help
ipv4 :: Parser IPv4
ipv4 = error "TODO: ipv4 :: Parser IPv4"

-- A country code is at least one alphabetical character
countryCode :: Parser CountryCode
countryCode = error "TODO: countryCode :: Parser CountryCode"

-- Parse the whole pipe-separated feed row:
-- Example: 10.169.7.1|10.169.7.255|UK
row :: Parser FeedRow
row = error "TODO: row :: Parser FeedRow"

feedFile :: FilePath
feedFile = "data/GeoFeed.dsv"

-- Let's parse the feed!
-- Consider using BS.readFile to load file's content and BS.lines to break it into lines
-- "parseOnly" function from https://hackage.haskell.org/package/attoparsec-0.13.1.0/docs/Data-Attoparsec-ByteString-Char8.html
-- will run a provided parser.
-- Print the results.
parseFeed :: IO ()
parseFeed = error "TODO: parseFeed :: IO ()"
