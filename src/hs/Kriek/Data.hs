{-# language DeriveGeneric, DeriveAnyClass #-}
module Kriek.Data where

import GHC.Generics (Generic)
import Data.HashMap.Strict
import Control.Monad.State.Strict
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.Scientific
import Data.Hashable (Hashable, hashWithSalt)

data Name = Name String
          | NSName String String
  deriving (Eq, Generic, Hashable)

instance Show Name where
  show (Name s) = s
  show (NSName n s) = s ++ '/':(show s)

name :: Name -> String
name (Name s) = s
name (NSName _ s) = s

hasNS :: Name -> Bool
hasNS (Name _) = False
hasNS _ = True

data Position = Position { filename :: String, line :: Word, col :: Word }
  deriving (Eq, Generic, Hashable)

type Meta a = HashMap a a

data AST a
  = ANil
  | AInt Integer
  | AFloat Scientific
  | AChar Char
  | ASymbol Name
  | AKeyword String
  | AString String
  | AList [a]
  | ATuple [a]
  | ARecord [(a,a)]
  deriving (Eq, Generic, Hashable)

isSymbol :: AST a -> Bool
isSymbol (ASymbol s) = True
isSymbol _ = False

isBareSymbol :: AST a -> Bool
isBareSymbol (ASymbol (Name s)) = True
isBareSymbol _ = False

instance Show a => Show (AST a) where
  show ANil = "nil"
  show (AInt i) = show i
  show (AFloat  f) = show f
  show (AChar   c) = show c
  show (AString s) = "\"" ++ s ++ "\""
  show (ASymbol  n) = show n
  show (AKeyword n) = ':':(show n)
  show (AList    l) = "(" ++ (intercalate ", " (fmap show l)) ++ ")"
  show (ATuple   l) = "[" ++ (intercalate ", " (fmap show l)) ++ "]"
  show (ARecord  l) = "{" ++ (intercalate ", " (fmap h l)) ++"}"
    where h (k,v) = (show k) ++ ' ':(show v)

data Form = Form (AST Form) (Maybe Position) (Maybe (Meta Form))
  deriving (Generic)

ast :: Form -> AST Form
ast (Form a _ _) = a

instance Show Form where
  show (Form a _ _) = show a

instance Eq Form where
  (Form a _ _) == (Form b _ _) = a == b

instance Hashable Form where
  hashWithSalt i (Form a _ _) = hashWithSalt i a
