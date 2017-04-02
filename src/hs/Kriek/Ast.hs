{-# language DeriveGeneric, DeriveAnyClass #-}
module Kriek.Ast where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.Scientific

type Meta = [RecItem]

data Name = Name String
          | NSName String String
  deriving (Eq, Generic, Hashable, Show)

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
  | AKeyword String
  | AString String
  | AList [Form]
  | ATuple [Form]
  | ARecord [RecItem]
  deriving (Eq, Generic, Hashable, Show)

instance Show Form where
  show (Form a _ _) = show a
