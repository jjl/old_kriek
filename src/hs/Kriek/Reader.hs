{-# language TupleSections #-}
module Kriek.Reader (form, program) where

import Data.HashMap.Strict
import Kriek.Data
import Kriek.Ir
import Control.Applicative
import Control.Lens ((.~))
import Control.Monad.State
import Data.HashMap.Strict as M
import Data.Scientific (isInteger, coefficient)
import Text.Megaparsec hiding (Token)
import Text.Megaparsec.String
import qualified Data.List.NonEmpty as LNE
import qualified Text.Megaparsec.Lexer as L
import Prelude hiding (any)

type NQAST = HashMap AST AST -> AST

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

kBareSym :: Parser NQAST
kBareSym = do s <- oneOf start <?> "symbol start character"
              r <- many (oneOf rest <?> "symbol character")
              return $ ASymbol (Nothing, s:r)
  where start = "abcdefghijklmnopqrstuvwxyz"
        rest = start ++ ""

kKeyword :: Parser NQAST
kKeyword = do _ <- char ':'
              s <- oneOf start <?> "keyword start character"
              r <- some (oneOf rest) <?> "keyword character"
              return $ AKeyword (s:r)
  where start = "abcdefghijklmnopqrstuvwxyz-"
        rest = "1234567890:" ++ start

kNum :: Parser NQAST
kNum = do n <- L.signed (return ()) L.number
          return $ if isInteger n
                      then AInt (coefficient n)
                      else AFloat n

kString :: Parser NQAST
kString = AString <$> (char '"' >> manyTill L.charLiteral (char '"'))

kChar :: Parser NQAST
kChar = string "#\\" >> AChar <$> L.charLiteral

kListy :: (Char, Char) -> Parser [AST]
kListy (s,e) = between (char s) (char e) h
  where h = trim $ sepBy form space

kList :: Parser NQAST
kList = AList <$> kListy ('(',')')

kTuple :: Parser NQAST
kTuple = ATuple <$> kListy ('[',']')

kMeta :: Parser (HashMap AST AST)
kMeta = do _ <- string "^{"
           ris <- sepBy kRecItem ws
           _ <- char '}'
           return $ fromList ris

kRecItem :: Parser (AST, AST)
kRecItem = do f1 <- form
              _ <- some ws
              f2 <- form
              return (f1,f2)

kNil :: Parser NQAST
kNil = string "nil" >> return (\_ -> ANil)

kAst :: Parser NQAST
kAst = kList <|> kTuple <|> kString <|> kKeyword <|> kChar <|> kNil <|> kNum <|> kBareSym

form :: Parser AST
form = do p <- sourcePos
          m <- many ws *> optional kMeta
          o <- many ws *> kAst
          return $ o $ maybe M.empty id m

program :: Parser [AST]
program = trim $ sepBy form mwsc
