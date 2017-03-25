{-# language DeriveGeneric, DeriveAnyClass #-}
module Kriek.Ast where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.Scientific

type Meta a = [RecItem a]

newtype Name = Name String
  deriving (Eq, Generic, Hashable)

data Position = Position { filename :: String, line :: Word, col :: Word }
  deriving (Eq, Generic, Hashable)

data Form a = Form (AST a) (Maybe Position) (Maybe (Meta a))
  deriving (Generic, Hashable)

instance Eq a => Eq (Form a) where
  (Form a _ _) == (Form b _ _) = a == b

type RecItem a = ((Name, Maybe Position), Form a)

data AST a
  = KNil
  | KInt Integer
  | KFloat Scientific
  | KChar Char
  | KSymbol Name
  | KKeyword Name
  | KString String
  | KList [Form a]
  | KTuple [Form a]
  | KRecord [RecItem a]
  | KRuntime a
  deriving (Eq, Generic, Hashable)

instance Show a => Show (Form a) where
  show (Form a _ _) = show a

instance Show a => Show (AST a) where
  show KNil = "nil"
  show (KInt i) = show i
  show (KFloat  f) = show f
  show (KChar   c) = show c
  show (KString s) = s
  show (KSymbol  (Name n)) = n
  show (KKeyword (Name n)) = ':':n
  show (KList    l) = "(" ++ (intercalate "," (fmap show l)) ++ ")"
  show (KTuple   l) = "[" ++ (intercalate "," (fmap show l)) ++ "]"
  show (KRecord  l) = "{" ++ (intercalate "," (fmap h l)) ++"}"
    where h (((Name k),_),(Form v _ _)) = ':':k ++ ' ':(show v)
  show (KRuntime a) = show a

