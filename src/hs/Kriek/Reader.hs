module Kriek.Reader (form, program) where

import Data.HashMap.Strict
import Kriek.Data
import Kriek.Ir
import Control.Applicative
import Control.Monad.State
import Data.Scientific (isInteger, coefficient)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.String
import qualified Data.List.NonEmpty as LNE
import qualified Text.Megaparsec.Lexer as L
import Prelude hiding (any)

ws :: Parser ()
ws = oneOf " \n" >> return ()

wsc :: Parser ()
wsc = many (ws <|> kComment) >> return ()

mwsc :: Parser ()
mwsc = some (ws <|> kComment) >> return ()

trim :: Parser a -> Parser a
trim p = wsc *> p <* wsc

forbidden :: String
forbidden = "\r\t\v"

spToPos :: SourcePos -> Position
spToPos (SourcePos n l c) = Position n (unPos l) (unPos c)

sourcePos :: Parser Position
sourcePos = (spToPos . LNE.head . statePos) `liftM` getParserState

kComment :: Parser ()
kComment = L.skipLineComment ";"

kBareSym :: Parser (AST Form)
kBareSym = do s <- oneOf start <?> "symbol start character"
              r <- many (oneOf rest <?> "symbol character")
              return $ ASymbol (Name $ s:r)
  where start = "abcdefghijklmnopqrstuvwxyz"
        rest = start ++ ""

kQSym :: Parser (AST Form)
kQSym = between (char '|') (char '|') h
  where h = (ASymbol . Name) <$> some (noneOf banned)
        banned = "|\n" ++ forbidden

kKeyword :: Parser (AST Form)
kKeyword = do _ <- char ':'
              s <- oneOf start <?> "keyword start character"
              r <- some (oneOf rest) <?> "keyword character"
              return $ AKeyword (s:r)
  where start = "abcdefghijklmnopqrstuvwxyz-"
        rest = "1234567890:" ++ start

kNum :: Parser (AST Form)
kNum = do n <- L.signed (return ()) L.number
          return $ if isInteger n
                      then AInt (coefficient n)
                      else AFloat n

kString :: Parser (AST Form)
kString = AString <$> (char '"' >> manyTill L.charLiteral (char '"'))

kChar :: Parser (AST Form)
kChar = string "#\\" >> AChar <$> L.charLiteral

kListy :: (Char, Char) -> Parser [Form]
kListy (s,e) = between (char s) (char e) h
  where h = trim $ sepBy form space

kList :: Parser (AST Form)
kList = AList <$> kListy ('(',')')

kTuple :: Parser (AST Form)
kTuple = ATuple <$> kListy ('[',']')

kMeta :: Parser (Meta Form)
kMeta = do _ <- string "^{"
           ris <- sepBy kRecItem ws
           _ <- char '}'
           return $ fromList ris

kRecItem :: Parser (Form, Form)
kRecItem = do f1 <- form
              _ <- some ws
              f2 <- form
              return (f1,f2)

kNil :: Parser (AST Form)
kNil = string "nil" >> return ANil

kAst :: Parser (AST Form)
kAst = kQSym <|> kList <|> kTuple <|> kString <|> kKeyword <|> kChar <|> kNil <|> kNum <|> kBareSym

form :: Parser Form
form = do p <- sourcePos
          m <- many ws *> optional kMeta
          o <- many ws *> kAst
          return $ Form o (Just p) m

program :: Parser [Form]
program = trim $ sepBy form mwsc
