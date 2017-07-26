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
brackets = error "TODO Implement brackets"

braces :: Parser a -> Parser a
braces = error "TODO Implement braces"

plainChar :: Parser Char
plainChar = error "TODO Implement plainChar"

escapedChar :: Parser Char
escapedChar = error "TODO Implement escapedChar"

litString :: Parser String
litString = error "TODO Implement litString"

litBool :: Parser Bool
litBool = error "TODO Implement litBool"

comma :: Parser ()
comma = error "TODO Implement comma"

array :: Parser [Json]
array = error "TODO Implement array"

field :: Parser (String, Json)
field = error "TODO Implement field"

object :: Parser [(String, Json)]
object = error "TODO Implement object"

nullKeyword :: Parser ()
nullKeyword = error "TODO Implement nullKeyword"

number :: Parser Double
number = error "TODO Implement number"

json :: Parser Json
json = error "TODO Implement json"
