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

comma :: Parser ()
comma = symbol "," *> pure ()

plainChar :: Parser Char
plainChar = satisfy (`notElem` escapees)

escapedChar :: Parser Char
escapedChar = char '\\' *>
  (   (char 'n'   *> pure '\n')
  <|> (char 'r'   *> pure '\r')
  <|> (char 't'   *> pure '\t')
  <|> (char '"'   *> pure '"' )
  <|> (char '\\'  *> pure '\\')
  )

litString :: Parser String
litString = char '"' *> many (plainChar <|> escapedChar) <* char '"'

litBool :: Parser Bool
litBool
  =   symbol "true"   *> pure True
  <|> symbol "false"  *> pure False

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

array :: Parser [Json]
array = brackets (json `sepBy` comma)

field :: Parser (String, Json)
field = (,) <$> (litString <* symbol ":") <*> json

object :: Parser [(String, Json)]
object = braces (field `sepBy` comma)

nullKeyword :: Parser ()
nullKeyword = symbol "null" *> pure ()

number :: Parser Double
number
  =   L.float
  <|> ((0-) <$> (char '-' *> L.float))

json :: Parser Json
json
  =   (JsonBool       <$> litBool     )
  <|> (JsonString     <$> litString   )
  <|> (JsonArray      <$> array       )
  <|> (JsonObject     <$> object      )
  <|> (JsonNumber     <$> number      )
  <|> (const JsonNull <$> nullKeyword )
