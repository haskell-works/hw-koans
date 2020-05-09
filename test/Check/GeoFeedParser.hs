{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Check.GeoFeedParser where

import Control.Arrow (left)
import Data.List     (intercalate)

import qualified Control.Monad.Fail               as P
import qualified Data.Attoparsec.ByteString       as P (skip)
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as BSC
import qualified Koan.GeoFeedParser               as K

import Hedgehog
import Hedgehog.Extra

import qualified Hedgehog.Gen   as G
import qualified Hedgehog.Range as R

{- HLINT ignore "Redundant do"        -}
{- HLINT ignore "Redundant bracket"   -}
{- HLINT ignore "Reduce duplication"  -}

prop_digitsToInt :: Property
prop_digitsToInt = property $ do
  x <- forAll $ G.int (R.linear 0 255)
  K.digitsToInt (show x) === x

prop_int :: Property
prop_int = property $ do
  x <- forAll $ G.int (R.linear 0 255)
  runParserShow K.int x === Right x

prop_int_fail_string :: Property
prop_int_fail_string = property $ do
  s <- forAll $ G.string (R.linear 1 3) G.alpha
  x <- forAll $ G.string (R.linear 1 10) G.alphaNum
  runParser K.int (BSC.pack $ s <> x) === Left "digit: Failed reading: satisfyWith"

prop_octet :: Property
prop_octet = property $ do
  x <- forAll $ G.int (R.linear 256 9999)
  let res = runParserShow K.octet x
  left (const "") res === Left ""

prop_dot :: Property
prop_dot = property $ do
  s <- forAll $ G.string (R.linear 1 3) G.alpha
  left (const "") (runParserStr K.dot s) === Left ""
  runParserStr K.dot ('.':s) === Right '.'

prop_separator :: Property
prop_separator = property $ do
  s <- forAll $ G.string (R.linear 1 3) G.alpha
  left (const "") (runParserStr K.separator s) === Left ""
  runParserStr K.separator ('|':s) === Right '|'

prop_ipv4 :: Property
prop_ipv4 = property $ do
  vs@[a,b,c,d] <- forAll $ G.list (R.singleton 4) (G.int (R.linear 0 255))
  s <- forAll $ G.string (R.linear 1 3) G.alpha
  let str = intercalate "." (show <$> vs)
  runParserStr K.ipv4 str === Right (K.IPv4 a b c d)
  left (const "") (runParserStr K.ipv4 (s <> str)) === Left ""

prop_countryCode :: Property
prop_countryCode = property $ do
  s <- forAll $ G.string (R.linear 1 3) G.alpha
  runParserStr K.countryCode s === Right (K.CountryCode s)

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)

runParserStr :: P.Parser b -> String -> Either String b
runParserStr p s = P.parseOnly p (BSC.pack s)

runParser :: P.Parser b -> BS.ByteString -> Either String b
runParser = P.parseOnly

runParserShow :: Show a => P.Parser b -> a -> Either String b
runParserShow p a = P.parseOnly p (BSC.pack $ show a)

ipToStr (K.IPv4 a b c d) =
  intercalate "." (show <$> [a,b,c,d])
