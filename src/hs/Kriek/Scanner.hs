{-# LANGUAGE Strict, DeriveGeneric, FlexibleInstances, TupleSections #-}
module Kriek.Scanner
  ( scan
  , Op(..), Import(..), Value
  ) where

import Control.Monad.Trans.Either
import GHC.Generics (Generic)
import Data.Either (partitionEithers)
import Data.Scientific (Scientific)
import Data.HashMap.Strict as HM
import Data.HashSet as HS
import Data.List (all)
import Kriek.Data
import Kriek.Ir
import Kriek.Types
import Prelude hiding (read, lookup)

type Scanner r = EitherT Error IO r

type Value = AST Op

data Import = Import  { iModule :: String
                      , iAs :: (Maybe String)
                      , iRefer :: [String] }
  deriving (Eq)

data Op = OValue Value
        | OImport Import
        | OIf Op Op Op
        | ODef String Op
        -- | OFn
        -- | ODo [Analysis]
        -- | OLet
        -- | OCallcc
        -- | ONew
        -- | OSet
        -- | ONs
        -- | ODeftype
        -- | ODefrecord
        -- | OQuote
        -- | OCase
        -- | OVar
        -- | OCall Analysis [Analysis]
  deriving (Eq)

demandBareSymbol :: String -> Form -> Scanner String
demandBareSymbol e (Form (ASymbol s) _ _) = if hasNS s
  then right (name s)
  else left (Expected $ "bare symbol (" ++ e ++ ")")
demandBareSymbol e _ = left (Expected $ "bare symbol (" ++ e ++ ")")

-- :as foo
importAs :: [Form] -> Scanner ([Form],Maybe String)
importAs ((Form (AKeyword "as") _ _):fs) =
  case fs of
    (Form (ASymbol s) _ _):fs' ->
      if hasNS s then left $ Expected "non-namespaced symbol as a namespace alias"
      else right $ (fs', Just $ name s) 
    [] -> left $ Expected "symbol naming the alias"
importAs fs = right (fs, Nothing)

-- :refer [foo bar baz]
importRefer :: [Form] -> Scanner ([Form],[String])
importRefer ((Form (AKeyword "refer") _ _):fs) =
  case fs of
    (Form (ATuple l) _ _):fs' ->
      (fs',) <$> mapM (demandBareSymbol "name to refer") l
    _ -> left $ Expected "tuple of non-namespaced symbols"
importRefer fs = right (fs, [])

-- import foo (? :as f) (? :refer [bar baz]
scanImport :: Form -> Scanner Op
scanImport f@(Form (AList (_:l)) p _) = case l of
  [] -> left $ Expected "symbol naming module"
  f2@(Form (ASymbol s) _ _):r ->
    if hasNS s then left $ Expected "non-namespaced symbol naming a module"
    else do
      (r2, alias) <- importAs r
      (r3, refer) <- importRefer r2
      case r3 of
        [] -> right (OImport $ Import (name s) alias refer)
        e:r4 -> left $ Unexpected (show e) -- TODO: better error message. maybe they got the order wrong or misspelled or whatever

-- if condn then else
scanIf :: Form -> Scanner Op
scanIf f@(Form (AList (_:l)) p _) =
  case l of
    [c,t,e] -> do [c',t',e'] <- mapM scanAny l
                  right $ OIf c' t' e'
    _ -> left $ Expected "three subexpressions: condition, then, else"

scanList :: Form -> Scanner Op
scanList f@(Form (AList l) p _) = case l of
  [] -> right $ OValue $ AList []
  (l1:ls) -> case l1 of
    f2@(Form (ASymbol (Name s)) p' _) -> case s of
      "import" -> scanImport f
      "if" -> scanIf f
      _ -> left $ Unimplemented
    _ -> (OValue . AList) <$> mapM scanAny ls
scanList f = left (Expected "List")

scanTuple :: Form -> Scanner Op
scanTuple f@(Form (ATuple t) p _) = (OValue . ATuple) <$> (mapM scanAny t)

scanAny :: Form -> Scanner Op
scanAny f@(Form a p _) = case a of
  ANil       -> right $ OValue ANil
  AInt i     -> right $ OValue (AInt i)
  AFloat f   -> right $ OValue (AFloat f)
  AChar ch   -> right $ OValue (AChar ch)
  ASymbol n  -> right $ OValue (ASymbol n)
  AKeyword k -> right $ OValue (AKeyword k)
  AList _    -> scanList f
  ATuple _   -> scanTuple f
  -- ARecord r -> scanRecord c f
  _ -> left $ Unimplemented

scan :: Form -> IO (Either Error Op)
scan f = runEitherT $ scanAny f

-- scanListType :: [Form] -> Scanner (Maybe Type)
-- scanListType f = return Nothing

-- scanTypeTag :: Form -> Scanner (Maybe Type)
-- scanTypeTag (Form a p m) = case a of
--   (ASymbol (Name s)) -> return $ Just $ TCon (Tycon s Star)
--   (AList l) -> scanListType l
