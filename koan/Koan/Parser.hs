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
  fmap = error "TODO: Implement fmap for ParseResult"

newtype Parser a = Parser
  { runParser :: String -> ParseResult a
  }

fail :: String -> Parser a
fail = error "TODO: Implement fail"

instance Functor Parser where
  fmap = error "TODO: Implement fmap for Parser"

instance Applicative Parser where
  pure a  = error "TODO: Implement pure for Parser"
  (<*>)   = error "TODO: Implement (<*>) for Parser"

instance Alternative Parser where
  empty = error "TODO: Implement empty for Parser"
  (<|>) = error "TODO: Implement (<|>) for Parser"

satisfy :: (Char -> Bool) -> Parser Char
satisfy = error "TODO: Implement satisfy"

char :: Char -> Parser Char
char c = error "TODO: Implement char"

notChar :: Char -> Parser Char
notChar c = error "TODO: Implement notChar"

anyChar :: Parser Char
anyChar = error "TODO: Implement anyChar"

skip :: (Char -> Bool) -> Parser ()
skip p = error "TODO: Implement skip"

peekChar :: Parser (Maybe Char)
peekChar = error "TODO: Implement peekChar"

peekChar' :: Parser Char
peekChar' = error "TODO: Implement peekChar'"

digit :: Parser Char
digit = error "TODO: Implement digit"

letter :: Parser Char
letter = error "TODO: Implement letter"

space :: Parser Char
space = error "TODO: Implement space"

string :: String -> Parser String
string = error "TODO: Implement string"

doubleQuoted :: Parser String
doubleQuoted = error "TODO: Implement doubleQuoted"
