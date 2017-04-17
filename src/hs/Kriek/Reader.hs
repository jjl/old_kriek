module Kriek.Reader (form, program) where

import Kriek.Ast
import Control.Applicative
import Control.Monad.State
import Data.Scientific (isInteger, coefficient)
import Text.Megaparsec
import Text.Megaparsec.String
import qualified Data.List.NonEmpty as LNE
import qualified Text.Megaparsec.Lexer as L

--
-- Source positions
--

spToPos :: SourcePos -> Position
spToPos (SourcePos n l c) = Position n (unPos l) (unPos c)

sourcePos :: Parser Position
sourcePos = (spToPos . LNE.head . statePos) `liftM` getParserState

--
-- Lexer
--

-- Space consumer
sc :: Parser ()
sc = L.space (void spaceChar) lineComment empty
  where lineComment  = L.skipLineComment ";"

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

scSep :: Parser a -> Parser [a]
scSep a = sepBy a sc

--
-- Syntax
--
-- Parses a identifier string
identifier :: Parser String
identifier = lexeme ((:) <$> firstChar <*> many nonFirstChar)
  where
    symbolChar = oneOf "!?+-*<=>?@^_~" <?> "symbol"
    firstChar = letterChar <|> symbolChar
    nonFirstChar = firstChar <|> numberChar

symbol :: String -> Parser ()
symbol s = void $ lexeme $ string s

kMeta :: Parser Meta
kMeta = symbol "^{" *> sepBy kRecItem sc <* symbol "}"

name :: Parser Name
name = lexeme $ try nsName <|> plainName
  where
    plainName = Name <$> identifier
    nsName = do
      ns <- identifier
      void $ char '/'
      ident <- identifier
      return $ NSName ns ident

kRecItem :: Parser RecItem
kRecItem = (,) <$> form <*> form

kNil :: Parser AST
kNil = ANil <$ string "nil"

kBareSym :: Parser AST
kBareSym = ASymbol <$> name

kKeyword :: Parser AST
kKeyword = AKeyword <$> (symbol ":" *> identifier)

kNum :: Parser AST
kNum = do n <- L.signed sc (lexeme L.number)
          return $ if isInteger n
                   then AInt (coefficient n)
                   else AFloat n

kChar :: Parser AST
kChar = string "#\\" >> AChar <$> L.charLiteral

kString :: Parser AST
kString = AString <$> (char '"' >> manyTill L.charLiteral (char '"'))

kListy :: (String, String) -> Parser [Form]
kListy (s,e) = between (symbol s) (symbol e) h
  where h = lexeme $ scSep form

kList :: Parser AST
kList = AList <$> kListy ("(",")")

kTuple :: Parser AST
kTuple = ATuple <$> kListy ("[","]")

kAst :: Parser AST
kAst = kNil <|> kKeyword <|> kBareSym <|> kNum <|>
       kString <|> kChar <|> kList <|> kTuple

form :: Parser Form
form = do p <- sourcePos
          m <- optional kMeta
          o <- kAst
          return $ Form o (Just p) m

program :: Parser [Form]
program = lexeme $ scSep form
