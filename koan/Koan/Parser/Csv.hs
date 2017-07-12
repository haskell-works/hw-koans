module Koan.Parser.Csv where

import Control.Applicative
import Control.Monad (void)
import Data.Char
import Text.Megaparsec hiding (crlf)
import Text.Megaparsec.Expr
import Text.Megaparsec.String hiding (crlf)

import qualified Text.Megaparsec.Lexer as L

enrolled :: Bool
enrolled = False

-- ABNF grammar:
--
--  file        = record *(LF record) [LF]
--  record      = field *(COMMA field)
--  field       = (escaped / non-escaped)
--  escaped     = DQUOTE *(TEXTDATA / COMMA / CR / LF / 2DQUOTE) DQUOTE
--  non-escaped = *TEXTDATA
--  COMMA       = %x2C
--  CR          = %x0D
--  DQUOTE      = %x22
--  LF          = %x0A
--  TEXTDATA    = %x20-21 / %x23-2B / %x2D-7E

parseCsv :: String -> Either (ParseError (Token String) Dec) [[String]]
parseCsv = error "TODO: Implement parseCsv"

file :: Parser [[String]]
file = error "TODO: Implement file"

record :: Parser [String]
record = error "TODO: Implement record"

field :: Parser String
field = error "TODO: Implement field"

escaped :: Parser String
escaped = error "TODO: Implement escaped"

nonEscaped :: Parser String
nonEscaped = error "TODO: Implement nonEscaped"

comma :: Parser Char
comma = error "TODO: Implement comma"

dquote :: Parser Char
dquote = error "TODO: Implement dquote"

lf :: Parser Char
lf = error "TODO: Implement lf"

textdata :: Parser Char
textdata = error "TODO: Implement textdata"
