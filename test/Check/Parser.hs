{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell   #-}

module Check.Parser where

import Control.Applicative
import Data.List
import Data.Semigroup
import Data.String

import qualified Control.Monad.Fail               as P
import qualified Data.Attoparsec.ByteString       as P (skip)
import qualified Data.Attoparsec.ByteString.Char8 as P
import qualified Data.ByteString                  as BS
import qualified Data.ByteString.Char8            as BSC
import qualified Koan.Parser                      as K

import Hedgehog
import Hedgehog.Extra

import qualified Hedgehog.Gen   as Gen
import qualified Hedgehog.Range as Range

{-# ANN module ("HLint: ignore Redundant do"        :: String) #-}
{-# ANN module ("HLint: ignore Reduce duplication"  :: String) #-}
{-# ANN module ("HLint: ignore Redundant bracket"   :: String) #-}

toKoanResult :: P.Result a -> K.ParseResult a
toKoanResult (P.Done _ a)   = K.ParseSuccess "" a
toKoanResult (P.Fail _ _ m) = if "Failed reading: " `isPrefixOf` m
  then K.ParseFailure (drop 16 m)
  else K.ParseFailure m
toKoanResult (P.Partial _)  = K.ParseFailure "Incomplete"

prop_fmap_result_op :: Property
prop_fmap_result_op = property $ do
  let pure' a = K.Parser $ \s -> K.ParseSuccess s a
  ea <- forAll $ Gen.element [Right 1, Right 2, Right 3, Left "Failed Reading: failed", Left "error"]
  let kp = either K.ParseFailure    (K.ParseSuccess "") ea :: K.ParseResult Int
  let pp = either (P.Fail "xxx" []) (P.Done         "") ea :: P.Result Int
  ((+1) <$> kp) === toKoanResult ((+1) <$> pp)

prop_fail :: Property
prop_fail = property $ do
  ea <- forAll $ Gen.element ["Failed Reading: failed", "error"]
  let ka = K.fail ea :: K.Parser Int
  let pa = P.fail ea :: P.Parser Int
  K.runParser ka "" === toKoanResult (P.parse pa "")

prop_fmap_op :: Property
prop_fmap_op = property $ do
  let pure' a = K.Parser $ \s -> K.ParseSuccess s a
  let fail' msg = K.Parser (const (K.ParseFailure msg))
  ea <- forAll $ Gen.element [Right 1, Right 2, Right 3, Left "Failed Reading: failed", Left "error"]
  let kp = either fail'  pure' ea :: K.Parser Int
  let pp = either P.fail pure  ea :: P.Parser Int
  K.runParser ((+1) <$> kp) "" === toKoanResult (P.parse ((+1) <$> pp) "")

prop_pure :: Property
prop_pure = property $ do
  let pure' a = K.Parser $ \s -> K.ParseSuccess s a
  let fail' msg = K.Parser (const (K.ParseFailure msg))
  ea <- forAll $ Gen.element [Right 1, Right 2, Right 3, Left "Failed Reading: failed", Left "error"]
  let ka = either fail'  pure ea :: K.Parser Int
  let pa = either P.fail pure ea :: P.Parser Int
  K.runParser ka "" === toKoanResult (P.parse pa "")

prop_apply_op :: Property
prop_apply_op = property $ do
  let fail' msg = K.Parser (const (K.ParseFailure msg))
  ea <- forAll $ Gen.element [Right 1, Right 2, Right 3, Left "Failed Reading: failed", Left "error"]
  eb <- forAll $ Gen.element [Right 1, Right 2, Right 3, Left "failed", Left "error"]
  let kb = either fail'  pure eb :: K.Parser Int
  let ka = either fail'  pure ea :: K.Parser Int
  let pa = either P.fail pure ea :: P.Parser Int
  let pb = either P.fail pure eb :: P.Parser Int
  K.runParser ((,) <$> ka <*> kb) "" === toKoanResult (P.parse ((,) <$> pa <*> pb) "")

prop_empty :: Property
prop_empty = property $ do
  ea <- forAll $ Gen.element ["abc", "def" :: String]
  let ka = empty :: K.Parser Int
  let pa = empty :: P.Parser Int
  K.runParser ka "" === toKoanResult (P.parse pa "")

prop_orElse_op :: Property
prop_orElse_op = property $ do
  let fail' msg = K.Parser (const (K.ParseFailure msg))
  ea <- forAll $ Gen.element [Right 1, Right 2, Right 3, Left "Failed Reading: failed", Left "error"]
  eb <- forAll $ Gen.element [Right 1, Right 2, Right 3, Left "failed", Left "error"]
  let ka = either fail'  pure ea :: K.Parser Int
  let kb = either fail'  pure eb :: K.Parser Int
  let pa = either P.fail pure ea :: P.Parser Int
  let pb = either P.fail pure eb :: P.Parser Int
  K.runParser (ka <|> kb) "" === toKoanResult (P.parse (pa <|> pb) "")

prop_satisfy :: Property
prop_satisfy = property $ do
  ea <- forAll $ Gen.element "abcdef"
  eb <- forAll $ Gen.element "abcdef"
  let ka = K.satisfy (== ea) :: K.Parser Char
  let pa = P.satisfy (== ea) :: P.Parser Char
  K.runParser ka (fromString [eb]) === toKoanResult (P.parse pa (fromString [eb]))

prop_char :: Property
prop_char = property $ do
  ea <- forAll $ Gen.element "abcdef"
  eb <- forAll $ Gen.element "abcdef"
  let ka = K.char ea :: K.Parser Char
  let pa = P.char ea :: P.Parser Char
  K.runParser ka (fromString [eb]) === toKoanResult (P.parse pa (fromString [eb]))

prop_notChar :: Property
prop_notChar = property $ do
  ea <- forAll $ Gen.element "abcdef"
  eb <- forAll $ Gen.element "abcdef"
  let ka = K.notChar ea :: K.Parser Char
  let pa = P.notChar ea :: P.Parser Char
  K.runParser ka (fromString [eb]) === toKoanResult (P.parse pa (fromString [eb]))

prop_anyChar :: Property
prop_anyChar = property $ do
  ea <- forAll $ Gen.string (Range.linear 0 10) (Gen.element "abcdef")
  let ka = K.anyChar :: K.Parser Char
  let pa = P.anyChar :: P.Parser Char
  K.runParser ka (fromString ea) === toKoanResult (P.parse pa (fromString ea))

prop_peekChar :: Property
prop_peekChar = property $ do
  ea <- forAll $ Gen.string (Range.linear 0 10) (Gen.element "abcdef")
  let ka = K.peekChar :: K.Parser (Maybe Char)
  let pa = P.peekChar :: P.Parser (Maybe Char)
  K.runParser ka (fromString ea) === toKoanResult (P.parse pa (fromString ea))

-- prop_skip :: Property
-- prop_skip = property $ do
--   ea <- forAll $ Gen.string (Range.linear 0 10) (Gen.element "abcdef")
--   eb <- forAll $ Gen.string (Range.linear 0 3 ) (Gen.element "abcdef")
--   let ka = K.skip (`elem`                     eb ) :: K.Parser ()
--   let pa = P.skip (`elem` BS.unpack (BSC.pack eb)) :: P.Parser ()
--   K.runParser ka (fromString ea) === toKoanResult (P.parse pa (fromString ea))

{-
skip :: (Char -> Bool) -> Parser ()
skip p = satisfy p *> pure ()

peekChar :: Parser (Maybe Char)
peekChar = Parser $ \s -> case s of
  (a:as) -> ParseSuccess s (Just a)
  _      -> ParseSuccess s Nothing

peekChar' :: Parser Char
peekChar' = Parser $ \s -> case s of
  (a:as) -> ParseSuccess s a
  _      -> ParseFailure "unexpected end of stream"

digit :: Parser Char
digit = satisfy (\c -> '0' <= c && c <= '9')

letter :: Parser Char
letter = satisfy isAlpha

space :: Parser Char
space = satisfy isSpace

string :: String -> Parser String
string text = Parser $ \s -> if text `isPrefixOf` s
  then ParseSuccess (drop (length text) s) text
  else ParseFailure ("expected string " <> show text)

doubleQuoted :: Parser String
doubleQuoted = char '"' *> many escapedChar <* char '"'
  where escapedChar = satisfy (`notElem` chars) <|> (char '\\' *> satisfy (`elem` chars))
        chars       = "\\\"\n\t\r"


-}



tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
