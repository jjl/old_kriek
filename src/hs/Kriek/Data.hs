{-# language DeriveGeneric, DeriveAnyClass, FlexibleInstances, MultiParamTypeClasses, RankNTypes #-}
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

newtype Fn m t = Fn ([t] -> m t)
  deriving (Generic)

instance Show (Fn m t) where
  show _ = "<function>"
instance Hashable (Fn m t) where
  hashWithSalt _ _ = 0

instance Eq (Fn m t) where
  _ == _ = False

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

class HasAST a where
  ast :: a -> AST a

-- class MaybeHasAST a where
--   mast :: a -> Maybe (AST a)
  
data Form = Form (AST Form) (Maybe Position) (Maybe (Meta Form))
  deriving (Generic)

instance HasAST Form where
  ast (Form a _ _) = a

instance Show Form where
  show (Form a _ _) = show a

instance Eq Form where
  (Form a _ _) == (Form b _ _) = a == b

instance Hashable Form where
  hashWithSalt i (Form a _ _) = hashWithSalt i a

data Error = Unimplemented
           | Unexpected String
           | Expected String
           | ExpectedSome [String]
           | ReboundAlias String String
           | Errors [Error]

juxt :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
juxt f1 f2 (a,c) = (f1 a, f2 c)

pair f = juxt f f

-- reform :: HasAST m b => (AST m a -> a) -> b -> AST m a
-- reform w b = case (ast b) of
--   ANil -> ANil
--   AInt i -> AInt i
--   AFloat f -> AFloat f
--   AChar c -> AChar c
--   ASymbol s -> ASymbol s
--   AKeyword k -> AKeyword k
--   AString s -> AString s
--   AList l -> AList $ fmap (w . reform w) l
--   ATuple t -> ATuple $ fmap (w . reform w) t
--   ARecord r -> ARecord $ fmap (pair $ w . reform w) r
