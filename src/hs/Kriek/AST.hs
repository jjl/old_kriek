{-# language DeriveGeneric, DeriveAnyClass #-}
module Kriek.AST where

import GHC.Generics (Generic)
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.Scientific

class MaybeHasPosition a where
  position :: a -> Maybe Position

class HasAST a where
  ast :: a -> AST

type Meta = [RecItem]

newtype Name = Name String
  deriving (Eq, Generic, Hashable, Show)

data Position = Position { filename :: String, line :: Word, col :: Word }
  deriving (Eq, Generic, Hashable, Show)

data Form = Form AST (Maybe Position) (Maybe Meta)
  deriving (Eq, Generic, Hashable, Show)

instance MaybeHasPosition Form where
  position (Form _ p _) = p
instance HasAST Form where
  ast (Form a _ _) = a

type RecItem = ((Name, Maybe Position), Form)

data AST = KNil
         | KInt Integer
         | KFloat Scientific
         | KChar Char
         | KSymbol Name
         | KKeyword Name
         | KString String
         | KList [Form]
         | KTuple [Form]
         | KRecord [RecItem]
         deriving (Eq, Generic, Hashable)

instance Show AST where
  show KNil = "nil"
  show (KInt i) = show i
  show (KFloat  f) = show f
  show (KChar   c) = show c
  show (KString s) = s
  show (KSymbol  (Name n)) = n
  show (KKeyword (Name n)) = ':':n
  show (KList    l) = "(" ++ (intercalate "," (fmap (show . ast) l)) ++ ")"
  show (KTuple   l) = "[" ++ (intercalate "," (fmap (show . ast) l)) ++ "]"
  show (KRecord  l) = "{" ++ (intercalate "," (fmap h l)) ++"}"
    where h (((Name k),_),(Form v _ _)) = ':':k ++ ' ':(show v)

