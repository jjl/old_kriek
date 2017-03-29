{-# language DeriveGeneric, DeriveAnyClass #-}
module Kriek.Ast where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.Scientific

type Meta = [RecItem]

data Name = Name String
          | NSName String String
  deriving (Eq, Generic, Hashable)

instance Show Name where
  show (Name s) = s
  show (NSName n s) = s ++ '/':(show s)

data Position = Position { filename :: String, line :: Word, col :: Word }
  deriving (Eq, Generic, Hashable)

data Form = Form AST (Maybe Position) (Maybe Meta)
  deriving (Generic, Hashable)

instance Eq Form where
  (Form a _ _) == (Form b _ _) = a == b

type RecItem = (Form, Form)

data AST
  = ANil
  | AInt Integer
  | AFloat Scientific
  | AChar Char
  | ASymbol Name
  | AKeyword Name
  | AString String
  | AList [Form]
  | ATuple [Form]
  | ARecord [RecItem]
  deriving (Eq, Generic, Hashable)

instance Show Form where
  show (Form a _ _) = show a

instance Show AST where
  show ANil = "nil"
  show (AInt i) = show i
  show (AFloat  f) = show f
  show (AChar   c) = show c
  show (AString s) = "\"" ++ s ++ "\""
  show (ASymbol  n) = show n
  show (AKeyword n) = ':':(show n)
  show (AList    l) = "(" ++ (intercalate ", " (fmap show l)) ++ ")"
  show (ATuple   l) = "[" ++ (intercalate ", " (fmap show l)) ++ "]"
  show (ARecord  l) = "{" ++ (intercalate ", " (fmap h l)) ++ "}"
    where h ((Form k _ _),(Form v _ _)) = (show k) ++ ' ':(show v)
