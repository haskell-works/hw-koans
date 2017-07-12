module Koan.Parser.Json where

import Control.Applicative
import Control.Monad          (void)
import Data.Char
import Text.Megaparsec        hiding (crlf)
import Text.Megaparsec.Expr
import Text.Megaparsec.String hiding (crlf)

import qualified Text.Megaparsec.Lexer as L

enrolled :: Bool
enrolled = False

data Json
  = JsonBool    Bool
  | JsonNumber  Double
  | JsonString  String
  | JsonArray   [Json]
  | JsonObject  [(String, Json)]
  | JsonNull
  deriving (Eq, Show)

sc :: Parser ()
sc = L.space (void spaceChar) lineCmnt blockCmnt
  where lineCmnt  = L.skipLineComment "//"
        blockCmnt = L.skipBlockComment "/*" "*/"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

escapees :: String
escapees = "\\\"\n\r\t"

-- Hint: Use between function
brackets :: Parser a -> Parser a
brackets = error "TODO Implement json"

braces :: Parser a -> Parser a
braces = error "TODO Implement json"

plainChar :: Parser Char
plainChar = error "TODO Implement json"

escapedChar :: Parser Char
escapedChar = error "TODO Implement json"

litString :: Parser String
litString = error "TODO Implement json"

litBool :: Parser Bool
litBool = error "TODO Implement json"

comma :: Parser ()
comma = error "TODO Implement json"

array :: Parser [Json]
array = error "TODO Implement json"

field :: Parser (String, Json)
field = error "TODO Implement json"

object :: Parser [(String, Json)]
object = error "TODO Implement json"

nullKeyword :: Parser ()
nullKeyword = error "TODO Implement json"

json :: Parser Json
json = error "TODO Implement json"
