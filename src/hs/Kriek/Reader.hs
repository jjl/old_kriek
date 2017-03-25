{-# language ViewPatterns #-}
module Kriek.Reader (form, program) where

import Kriek.Ast
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

wscSep :: Parser a -> Parser [a]
wscSep a = sepBy a mwsc

trim :: Parser a -> Parser a
trim p = wsc *> p <* wsc

sp :: Parser ()
sp = char ' ' >> return ()

forbidden :: String
forbidden = "\r\t\v"

spToPos :: SourcePos -> Position
spToPos (SourcePos n l c) = Position n (unPos l) (unPos c)

sourcePos :: Parser Position
sourcePos = (spToPos . LNE.head . statePos) `liftM` getParserState

kComment :: Parser ()
kComment = do _ <- char ';' <?> "comment marker"
              _ <- many (satisfy h) <?> ""
              _ <- char '\n'
              return ()
  where h x = (fromEnum x) > 0x7f
              
kBareSym :: Parser (AST a)
kBareSym = do s <- oneOf start <?> "symbol start character"
              r <- many (oneOf rest <?> "symbol character")
              return $ KSymbol (Name $ s:r)
  where start = "abcdefghijklmnopqrstuvwxyz"
        rest = start ++ ""

kQSym :: Parser (AST a)
kQSym = between (char '|') (char '|') h
  where h = (KSymbol . Name) <$> some (noneOf banned)
        banned = "|\n" ++ forbidden

kKeyword :: Parser (AST a)
kKeyword = do _ <- char ':'
              s <- oneOf start <?> "keyword start character"
              r <- some (oneOf rest) <?> "keyword character"
              return $ KKeyword (Name $ s:r)
  where start = "abcdefghijklmnopqrstuvwxyz-"
        rest = "1234567890:" ++ start

kNum :: Parser (AST a)
kNum = do n <- L.signed (return ()) L.number
          return $ case (isInteger n) of
            True  -> KInt (coefficient n)
            False -> KFloat n

kString :: Parser (AST a)
kString = KString <$> between (char '"') (char '"') (many L.charLiteral)

kChar :: Parser (AST a)
kChar = string "#\\" >> KChar <$> L.charLiteral

kListy :: (Char, Char) -> Parser [Form a]
kListy (s,e) = between (char s) (char e) h
  where h = trim $ sepBy form mwsc

kList :: Parser (AST a)
kList = KList <$> kListy ('(',')')

kTuple :: Parser (AST a)
kTuple = KTuple <$> kListy ('[',']')

kRecItem :: Parser (RecItem a)
kRecItem = do p <- sourcePos
              (KKeyword n) <- kKeyword
              _ <- skipSome sp
              f <- form
              return ((n, Just p), f)

kMeta :: Parser (Maybe (Meta a))
kMeta = do _ <- string "^{" >> wsc
           ris <- wscSep kRecItem
           _ <- skipMany ws >> char '}'
           return $ case ris of
             [] -> Nothing
             _ -> Just ris
           
kNil :: Parser (AST a)
kNil = string "nil" >> return KNil

kAst :: Parser (AST a)
kAst = kQSym <|> kList <|> kTuple <|> kString <|> kKeyword <|> kChar <|> kNil <|> kNum <|> kBareSym

form :: Parser (Form a)
form = do p <- sourcePos
          m <- kMeta
          o <- kAst <* wsc
          return $ Form o (Just p) m

program :: Parser [Form a]
program = trim $ wscSep form
