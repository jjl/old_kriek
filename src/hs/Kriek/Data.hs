{-# language DeriveGeneric, DeriveAnyClass, FlexibleInstances, MultiParamTypeClasses, RankNTypes, TemplateHaskell #-}
module Kriek.Data where

import Control.Lens (makeLenses, (^.), (.~), (%~),_1, _2, Lens'(..))
import GHC.Generics (Generic)
import Data.HashMap.Strict as M
import Control.Monad.Trans.Either
import Control.Monad.State.Strict
import Data.Hashable (Hashable)
import Data.List (intercalate)
import Data.Scientific
import Data.Hashable (Hashable, hashWithSalt)

data Position = Position { _filename :: String
                         , _line :: Word
                         , _col :: Word }
  deriving (Eq, Generic, Hashable)

type SameMap a = HashMap a a

type ASTMap = SameMap AST
type RTMap = SameMap RT

newtype Fn m t = Fn ([t] -> m t)
  deriving (Generic)

instance Show (Fn m t) where
  show _ = "<function>"
instance Hashable (Fn m t) where
  hashWithSalt _ _ = 0

instance Eq (Fn m t) where
  _ == _ = False

data AST
  = ANil
  | AInt     { _aint :: Integer, _ameta :: ASTMap }
  | AFloat   { _afloat :: Scientific, _ameta :: ASTMap }
  | AChar    { _achar :: Char, _ameta :: ASTMap }
  | ASymbol  { _asymbol :: (Maybe String, String), _ameta :: ASTMap }
  | AKeyword { _akeyword :: String, _ameta :: ASTMap }
  | AString  { _astring :: String, _ameta :: ASTMap }
  | AList    { _alist :: [AST], _ameta :: ASTMap }
  | ATuple   { _atuple :: [AST], _ameta :: ASTMap }
  | ARecord  { _arecord :: [(AST,AST)], _ameta :: ASTMap }
  deriving (Eq, Generic, Hashable)

instance Show AST where
  show ANil = "nil"
  show (AInt i _) = show i
  show (AFloat  f _) = show f
  show (AChar   c _) = show c
  show (AString s _) = "\"" ++ s ++ "\""
  show (ASymbol  n _) = show n
  show (AKeyword n _) = ':':(show n)
  show (AList    l _) = "(" ++ (intercalate ", " (fmap show l)) ++ ")"
  show (ATuple   l _) = "[" ++ (intercalate ", " (fmap show l)) ++ "]"
  show (ARecord  l _) = "{" ++ (intercalate ", " (fmap h l)) ++"}"
    where h (k,v) = (show k) ++ ' ':(show v)

data RT
  = RNil
  | RBool    { _rbool :: Bool, _rmeta :: RTMap }
  | RInt     { _rint :: Integer, _rmeta :: RTMap }
  | RFloat   { _rfloat :: Scientific, _rmeta :: RTMap }
  | RChar    { _rchar :: Char, _rmeta :: RTMap }
  | RSymbol  { _rsymbol :: (Maybe String, String), _rmeta :: RTMap }
  | RKeyword { _rkeyword :: String, _rmeta :: RTMap }
  | RString  { _rstring :: String, _rmeta :: RTMap }
  | RList    { _rlist :: [Op], _rmeta :: RTMap }
  | RTuple   { _rtuple :: [Op], _rmeta :: RTMap }
  | RRecord  { _rrecord :: [(Op,Op)], _rmeta :: RTMap }
  | RFn      { _rfn :: Fn Runtime RT, _rmeta :: RTMap }
  | RMacro   { _rmacro :: Fn Runtime RT, _rmeta :: RTMap }
  deriving (Eq, Generic, Hashable)

-- FIXME: Wrote this to shut GHC up when writing findMacro
-- Complains about lack of a monoid instance for RT because of namedModule
-- I have no idea why it's required, it doesn't make sense
-- I have no idea what a correct mappend for this type would be
-- I hope it's not causing a subtle bug --jjl
instance Monoid RT where
  mempty = RNil
  mappend RNil r = r
  mappend l RNil = l

astMapToRtMap :: ASTMap -> RTMap
astMapToRtMap m = M.fromList $ fmap h $ M.toList m
  where h (k,v) = (astToRt k, astToRt v)

rtMapToAstMap :: RTMap -> Either RT ASTMap
rtMapToAstMap m = do l <- mapM h $ M.toList m
                     Right $ M.fromList l
  where h (k,v) = do k' <- rtToAst k
                     v' <- rtToAst v
                     Right (k',v')

astToRt :: AST -> RT
astToRt a = case a of
  ANil -> RNil
  (AInt i m) -> RInt i (astMapToRtMap m)
  (AFloat  f m) -> RFloat f (astMapToRtMap m)
  (AChar   c m) -> RChar c (astMapToRtMap m)
  (AString s m) -> RString s (astMapToRtMap m)
  (ASymbol  n m) -> RSymbol n (astMapToRtMap m)
  (AKeyword n m) -> RKeyword n (astMapToRtMap m)
  (AList    l m) -> RList (fmap (RT . astToRt) l) (astMapToRtMap m)
  (ATuple   l m) -> RTuple (fmap (RT . astToRt) l)(astMapToRtMap m)
  (ARecord  l m) -> RRecord (fmap h l) (astMapToRtMap m)
    where h (k,v) = (RT $ astToRt k, RT $ astToRt v)

rtToAst :: RT -> Either RT AST
rtToAst a = case a of
  RNil           -> Right $ ANil
  (RInt i m)     -> AInt i <$> (rtMapToAstMap m)
  (RFloat  f m)  -> AFloat f <$> (rtMapToAstMap m)
  (RChar   c m)  -> AChar c <$> (rtMapToAstMap m)
  (RString s m)  -> AString s <$> (rtMapToAstMap m)
  (RSymbol  n m) -> ASymbol n <$> (rtMapToAstMap m)
  (RKeyword n m) -> AKeyword n <$> (rtMapToAstMap m)
  -- (RList    l m) -> AList <$> (mapM (ro) l) (rtMapToAstMap m)
  -- (RTuple   l m) -> ATuple (fmap (RT . astToRt) l)(rtMapToAstMap m)
  -- (RRecord  l m) -> ARecord (fmap h l) (rtMapToAstMap m)
    where h (k,v) = (RT $ astToRt k, RT $ astToRt v)

data Op
  = RT RT
  | ONil
  | OIf Op Op Op
  | ODef String Op
  | OFun (Maybe String)
  | OImport Import
  | OLet (String, Op) [Op]
  | ORefLexical String
  | ORefRefer String
  | ORefDef String String
  -- | OFn
  -- | ODo [Analysis]
  -- | OLet
  -- | ONew
  -- | OSet
  -- | ODeftype
  -- | ODefrecord
  -- | OQuote
  -- | OCase
  -- | OVar
  -- | OCall Analysis [Analysis]
  deriving (Eq, Generic, Hashable, Show)

ropToRt (RT r) = Right r
ropToRt r = Left r

instance Show RT where
  show RNil = "nil"
  show (RInt i _) = show i
  show (RFloat  f _) = show f
  show (RChar   c _) = show c
  show (RString s _) = "\"" ++ s ++ "\""
  show (RSymbol  n _) = show n
  show (RKeyword n _) = ':':(show n)
  show (RList    l _) = "(" ++ (intercalate ", " (fmap show l)) ++ ")"
  show (RTuple   l _) = "[" ++ (intercalate ", " (fmap show l)) ++ "]"
  show (RRecord  l _) = "{" ++ (intercalate ", " (fmap h l)) ++"}"
    where h (k,v) = (show k) ++ ' ':(show v)
  show (RFn _ _) = "<function>"

type Runtime = EitherT Error (StateT Context IO)

data Error = Unimplemented
           | Unexpected String
           | Expected String
           | ExpectedSome [String]
           | ReboundAlias String String
           | UndefinedRef String
           | Errors [Error]

type QName = (Maybe String, String)

data Scope = Scope { _recursive :: HashMap String RT }

type NameMap = HashMap String

newScope = Scope empty

data Module = Module
  { _imports :: NameMap String
  , _refers :: NameMap String
  , _defs :: NameMap RT }

newModule = Module empty empty empty

data Global = Global
  { _modules :: HashMap String Module
  , _classpath :: [String] }

newGlobal :: String -> [String] -> Global
newGlobal m cp = Global (singleton m newModule) cp

data Local = Local
  { _cur_module :: String
  , _lexicals :: Scope }

newLocal :: String -> Local
newLocal = (flip Local) newScope

type Context = (Global, Local)

newContext :: String -> [String] -> Context
newContext m cp = (newGlobal m cp, newLocal m)

data Import = Import  { iModule :: String
                      , iAs :: (Maybe String)
                      , iRefer :: [String] }
  deriving (Eq, Generic, Hashable, Show)

  -- deriving (Eq)

makeLenses ''AST
makeLenses ''RT
makeLenses ''Position
makeLenses ''Scope
makeLenses ''Local
makeLenses ''Global
makeLenses ''Module

juxt :: (a -> b) -> (c -> d) -> (a,c) -> (b,d)
juxt f1 f2 (a,c) = (f1 a, f2 c)

pair f = juxt f f

