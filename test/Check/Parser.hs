{-# LANGUAGE OverloadedStrings    #-}
{-# LANGUAGE TemplateHaskell      #-}
{-# LANGUAGE FlexibleInstances    #-}

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

data ParsedResult a
  = ParsedSuccess a
  | ParsedFailure String
  deriving (Eq, Show)

class FromParsedResult f where
  toParsedResult :: f a -> ParsedResult a

instance FromParsedResult P.Result where
  toParsedResult (P.Done _ a)   = ParsedSuccess a
  toParsedResult (P.Fail _ _ m) = if "Failed reading: " `isPrefixOf` m
    then ParsedFailure (drop 16 m)
    else ParsedFailure m
  toParsedResult (P.Partial _)  = ParsedFailure "Incomplete"

instance FromParsedResult K.ParseResult where
  toParsedResult (K.ParseSuccess _ a) = ParsedSuccess a
  toParsedResult (K.ParseFailure m)   = ParsedFailure m

instance FromParsedResult ParsedResult where
  toParsedResult = id

infixl 1 =~=

(=~=) :: (Monad m, FromParsedResult f, FromParsedResult g, Eq a, Show a) => f a -> g a -> Test m ()
(=~=) fa ga = toParsedResult fa === toParsedResult ga

prop_fmap_result_op :: Property
prop_fmap_result_op = property $ do
  let pure' a = K.Parser $ \s -> K.ParseSuccess s a
  ea <- forAll $ Gen.element [Right 1, Right 2, Right 3, Left "Failed Reading: failed", Left "error"]
  let kp = either K.ParseFailure    (K.ParseSuccess "") ea :: K.ParseResult Int
  let pp = either (P.Fail "xxx" []) (P.Done         "") ea :: P.Result Int
  (+1) <$> kp =~= (+1) <$> pp

prop_fail :: Property
prop_fail = property $ do
  ea <- forAll $ Gen.element ["Failed Reading: failed", "error"]
  let ka = K.fail ea :: K.Parser Int
  let pa = P.fail ea :: P.Parser Int
  K.runParser ka "" =~= P.parse pa ""

prop_fmap_op :: Property
prop_fmap_op = property $ do
  let pure' a = K.Parser $ \s -> K.ParseSuccess s a
  let fail' msg = K.Parser (const (K.ParseFailure msg))
  ea <- forAll $ Gen.element [Right 1, Right 2, Right 3, Left "Failed Reading: failed", Left "error"]
  let kp = either fail'  pure' ea :: K.Parser Int
  let pp = either P.fail pure  ea :: P.Parser Int
  K.runParser ((+1) <$> kp) "" =~= P.parse ((+1) <$> pp) ""

prop_pure :: Property
prop_pure = property $ do
  let pure' a = K.Parser $ \s -> K.ParseSuccess s a
  let fail' msg = K.Parser (const (K.ParseFailure msg))
  ea <- forAll $ Gen.element [Right 1, Right 2, Right 3, Left "Failed Reading: failed", Left "error"]
  let ka = either fail'  pure ea :: K.Parser Int
  let pa = either P.fail pure ea :: P.Parser Int
  K.runParser ka "" =~= P.parse pa ""

prop_apply_op :: Property
prop_apply_op = property $ do
  let fail' msg = K.Parser (const (K.ParseFailure msg))
  ea <- forAll $ Gen.element [Right 1, Right 2, Right 3, Left "Failed Reading: failed", Left "error"]
  eb <- forAll $ Gen.element [Right 1, Right 2, Right 3, Left "failed", Left "error"]
  let kb = either fail'  pure eb :: K.Parser Int
  let ka = either fail'  pure ea :: K.Parser Int
  let pa = either P.fail pure ea :: P.Parser Int
  let pb = either P.fail pure eb :: P.Parser Int
  K.runParser ((,) <$> ka <*> kb) "" =~= P.parse ((,) <$> pa <*> pb) ""

prop_empty :: Property
prop_empty = property $ do
  ea <- forAll $ Gen.element ["abc", "def" :: String]
  let ka = empty :: K.Parser Int
  let pa = empty :: P.Parser Int
  K.runParser ka "" =~= P.parse pa ""

prop_orElse_op :: Property
prop_orElse_op = property $ do
  let fail' msg = K.Parser (const (K.ParseFailure msg))
  ea <- forAll $ Gen.element [Right 1, Right 2, Right 3, Left "Failed Reading: failed", Left "error"]
  eb <- forAll $ Gen.element [Right 1, Right 2, Right 3, Left "failed", Left "error"]
  let ka = either fail'  pure ea :: K.Parser Int
  let kb = either fail'  pure eb :: K.Parser Int
  let pa = either P.fail pure ea :: P.Parser Int
  let pb = either P.fail pure eb :: P.Parser Int
  K.runParser (ka <|> kb) "" =~= P.parse (pa <|> pb) ""

prop_satisfy :: Property
prop_satisfy = property $ do
  ea <- forAll $ Gen.element "abcdef"
  eb <- forAll $ Gen.element "abcdef"
  let ka = K.satisfy (== ea) :: K.Parser Char
  let pa = P.satisfy (== ea) :: P.Parser Char
  K.runParser ka (fromString [eb]) =~= P.parse pa (fromString [eb])

prop_char :: Property
prop_char = property $ do
  ea <- forAll $ Gen.element "abcdef"
  eb <- forAll $ Gen.element "abcdef"
  let ka = K.char ea :: K.Parser Char
  let pa = P.char ea :: P.Parser Char
  K.runParser ka (fromString [eb]) =~= P.parse pa (fromString [eb])

prop_notChar :: Property
prop_notChar = property $ do
  ea <- forAll $ Gen.element "abcdef"
  eb <- forAll $ Gen.element "abcdef"
  let ka = K.notChar ea :: K.Parser Char
  let pa = P.notChar ea :: P.Parser Char
  K.runParser ka (fromString [eb]) =~= P.parse pa (fromString [eb])

prop_anyChar :: Property
prop_anyChar = property $ do
  ea <- forAll $ Gen.string (Range.linear 0 10) (Gen.element "abcdef")
  let ka = K.anyChar :: K.Parser Char
  let pa = P.anyChar :: P.Parser Char
  K.runParser ka (fromString ea) =~= P.parse pa (fromString ea)

prop_peekChar :: Property
prop_peekChar = property $ do
  ea <- forAll $ Gen.string (Range.linear 0 10) (Gen.element "abcdef")
  let ka = K.peekChar :: K.Parser (Maybe Char)
  let pa = P.peekChar :: P.Parser (Maybe Char)
  K.runParser ka (fromString ea) =~= P.parse pa (fromString ea)

prop_skip :: Property
prop_skip = property $ do
  ea <- forAll $ Gen.string (Range.linear 0 10) (Gen.element "abcdef")
  eb <- forAll $ Gen.string (Range.linear 1 3 ) (Gen.element "abcdef")
  let ka = K.skip (`elem`                     eb ) :: K.Parser ()
  let pa = P.skip (`elem` BS.unpack (BSC.pack eb)) :: P.Parser ()
  K.runParser ka (fromString ea) =~= P.parse pa (fromString ea)

prop_digit :: Property
prop_digit = property $ do
  ea <- forAll $ Gen.string (Range.linear 0 2) (Gen.element "ABCDEFabcdef0123 ")
  K.runParser K.digit (fromString ea) =~= P.parse P.digit (fromString ea)

prop_letter :: Property
prop_letter = property $ do
  ea <- forAll $ Gen.string (Range.linear 0 2) (Gen.element "ABCDEFabcdef0123 ")
  K.runParser K.letter (fromString ea) =~= P.parse P.letter_ascii (fromString ea)

prop_space :: Property
prop_space = property $ do
  ea <- forAll $ Gen.string (Range.linear 0 2) (Gen.element "ABCDEFabcdef0123 ")
  K.runParser K.space (fromString ea) =~= P.parse P.space (fromString ea)

prop_string :: Property
prop_string = property $ do
  ea <- forAll $ Gen.string (Range.linear 0 10) (Gen.element "10")
  eb <- forAll $ Gen.string (Range.linear 0  3) (Gen.element "10")
  K.runParser (K.string eb) (fromString ea) =~= BSC.unpack <$> P.parse (P.string (fromString eb)) (fromString ea)

prop_doubleQuoted :: Property
prop_doubleQuoted = property $ do
  ea <- forAll $ Gen.string (Range.linear 0 10) (Gen.element "abc\"\\")
  eb <- forAll $ Gen.choice [pure (show ea)]
  K.runParser K.doubleQuoted (fromString eb) =~= ParsedSuccess ea

{-
doubleQuoted :: Parser String
doubleQuoted = char '"' *> many escapedChar <* char '"'
  where escapedChar = satisfy (`notElem` chars) <|> (char '\\' *> satisfy (`elem` chars))
        chars       = "\\\"\n\t\r"
-}

tests :: IO Bool
tests = checkSequential $ reversed $$(discover)
