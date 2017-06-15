module Koan.Parser where

import Control.Applicative
import Data.Char
import Data.List
import Data.Semigroup

import Prelude hiding (fail)

enrolled :: Bool
enrolled = False

data ParseResult a
  = ParseSuccess String a
  | ParseFailure String
  deriving (Eq, Show)

instance Functor ParseResult where
  f `fmap` ParseSuccess remainder value = ParseSuccess remainder (f value)
  f `fmap` ParseFailure msg             = ParseFailure msg

newtype Parser a = Parser
  { runParser :: String -> ParseResult a
  }

fail :: String -> Parser a
fail msg = Parser (const (ParseFailure msg))

instance Functor Parser where
  f `fmap` Parser run = Parser (\s -> f `fmap` run s)

instance Applicative Parser where
  pure a = Parser $ \s -> ParseSuccess s a
  Parser runF <*> Parser runA = Parser $ \s -> case runF s of
    ParseSuccess t f -> case runA t of
      ParseSuccess u a -> ParseSuccess u (f a)
      ParseFailure msg -> ParseFailure msg
    ParseFailure msg -> ParseFailure msg

instance Alternative Parser where
  empty = fail "empty"
  Parser runL <|> Parser runR = Parser $ \s -> case runL s of
    ParseSuccess t lt -> ParseSuccess t lt
    ParseFailure _    -> case runR s of
      ParseSuccess t lt -> ParseSuccess t lt
      ParseFailure msg  -> ParseFailure msg

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser go
  where go (a:as) = if p a
          then ParseSuccess as a
          else ParseFailure "satisfyWith"
        go [] = ParseFailure "Incomplete"

char :: Char -> Parser Char
char c = satisfy (== c)

notChar :: Char -> Parser Char
notChar c = satisfy (/= c)

anyChar :: Parser Char
anyChar = satisfy (const True)

peekChar :: Parser (Maybe Char)
peekChar = Parser $ \s -> case s of
  (a:as) -> ParseSuccess "" (Just a)
  _      -> ParseFailure "Incomplete"

skip :: (Char -> Bool) -> Parser ()
skip p = Parser $ \s -> case runParser (satisfy p *> pure ()) s of
  ParseFailure "satisfyWith" -> ParseFailure "skip"
  result                     -> result

digit :: Parser Char
digit = satisfy (\c -> '0' <= c && c <= '9')

letter :: Parser Char
letter = satisfy isAlpha

space :: Parser Char
space = satisfy isSpace

string :: String -> Parser String
string s = Parser $ \t -> if s `isPrefixOf` t
  then ParseSuccess (drop (length s) t) s
  else if take (length t) s `isPrefixOf` t
    then ParseFailure "Incomplete"
    else ParseFailure "string"

doubleQuoted :: Parser String
doubleQuoted = char '"' *> many escapedChar <* char '"'
  where escapedChar = satisfy (`notElem` chars) <|> (char '\\' *> satisfy (`elem` chars))
        chars       = "\\\"\n\t\r" :: String
