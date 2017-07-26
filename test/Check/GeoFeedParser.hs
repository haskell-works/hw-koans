{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Check.GeoFeedParser where

import Control.Arrow  (left)
import Data.List      (intercalate)
import Data.Semigroup ((<>))

import qualified Control.Monad.Fail               as P
import qualified Data.Attoparsec.ByteString       as P (skip)
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as BSC
import qualified Koan.GeoFeedParser               as K

import Hedgehog
import Hedgehog.Extra

import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

prop_digitsToInt :: Property
prop_digitsToInt = property $ do
  x <- forAll $ Gen.int (Range.linear 0 255)
  K.digitsToInt (show x) === x

prop_int :: Property
prop_int = property $ do
  x <- forAll $ Gen.int (Range.linear 0 255)
  runParserShow K.int x === Right x

prop_int_fail_string :: Property
prop_int_fail_string = property $ do
  s <- forAll $ Gen.string (Range.linear 1 3) Gen.alpha
  x <- forAll $ Gen.string (Range.linear 1 10) Gen.alphaNum
  runParser K.int (BSC.pack $ s <> x) === Left "digit: Failed reading: satisfyWith"

prop_octet :: Property
prop_octet = property $ do
  x <- forAll $ Gen.int (Range.linear 256 9999)
  let res = runParserShow K.octet x
  left (const "") res === Left ""

prop_dot :: Property
prop_dot = property $ do
  s <- forAll $ Gen.string (Range.linear 1 3) Gen.alpha
  left (const "") (runParserStr K.dot s) === Left ""
  runParserStr K.dot ('.':s) === Right '.'

prop_separator :: Property
prop_separator = property $ do
  s <- forAll $ Gen.string (Range.linear 1 3) Gen.alpha
  left (const "") (runParserStr K.separator s) === Left ""
  runParserStr K.separator ('|':s) === Right '|'

prop_ipv4 :: Property
prop_ipv4 = property $ do
  vs@[a,b,c,d] <- forAll $ Gen.list (Range.singleton 4) (Gen.int (Range.linear 0 255))
  s <- forAll $ Gen.string (Range.linear 1 3) Gen.alpha
  let str = intercalate "." (show <$> vs)
  runParserStr K.ipv4 str === Right (K.IPv4 a b c d)
  left (const "") (runParserStr K.ipv4 (s <> str)) === Left ""

prop_countryCode :: Property
prop_countryCode = property $ do
  s <- forAll $ Gen.string (Range.linear 1 3) Gen.alpha
  runParserStr K.countryCode s === Right (K.CountryCode s)

prop_row :: Property
prop_row = property $ do
  ip1 <- forAll genIp
  ip2 <- forAll genIp
  s <- forAll $ Gen.string (Range.linear 1 3) Gen.alpha
  let str = intercalate "|" [ipToStr ip1, ipToStr ip2, s]
  runParserStr K.row str === Right (K.FeedRow ip1 ip2 (Just $ K.CountryCode s))

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)

runParserStr :: P.Parser b -> String -> Either String b
runParserStr p s = P.parseOnly p (BSC.pack s)

runParser :: P.Parser b -> BS.ByteString -> Either String b
runParser = P.parseOnly

runParserShow :: Show a => P.Parser b -> a -> Either String b
runParserShow p a = P.parseOnly p (BSC.pack $ show a)

genIp :: MonadGen m => m K.IPv4
genIp = do
  [a,b,c,d] <- Gen.list (Range.singleton 4) (Gen.int (Range.linear 0 255))
  return $ K.IPv4 a b c d

ipToStr (K.IPv4 a b c d) =
  intercalate "." (show <$> [a,b,c,d])
