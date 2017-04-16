{-# LANGUAGE Strict, DeriveGeneric, FlexibleInstances, TupleSections #-}
module Kriek.Scanner
  ( scan
  , Op(..), RT(..), Import(..)
  ) where

import Control.Lens (makeLenses, (^.), (.~), (%~),_1, _2, Lens'(..))
import Control.Monad.State.Strict
import Control.Monad.Trans.Either
import GHC.Generics (Generic)
import Data.Either (partitionEithers)
import Data.Scientific (Scientific)
import Data.HashMap.Strict as M
import Data.HashSet as S
import Data.List (all)
import Kriek.Data
import Kriek.Ir
import Kriek.Types
import Prelude hiding (read, lookup)

demandBareSymbol :: String -> AST -> Runtime String
demandBareSymbol e (ASymbol (Nothing, s) _) = return s
demandBareSymbol e _ = left (Expected $ "bare symbol (" ++ e ++ ")")

-- :as foo
importAs :: [AST] -> Runtime ([AST],Maybe String)
importAs ((AKeyword "as" _):fs) =
  case fs of
    (ASymbol (Nothing,s) _):fs' -> right $ (fs', Just s)
    [] -> left $ Expected "non-namespaced symbol aliasing the namespace"
importAs fs = right (fs, Nothing)

-- :refer [foo bar baz]
importRefer :: [AST] -> Runtime ([AST],[String])
importRefer ((AKeyword "refer" _):fs) =
  case fs of
    (ATuple l _):fs' ->
      (fs',) <$> mapM (demandBareSymbol "name to refer") l
    _ -> left $ Expected "tuple of non-namespaced symbols"
importRefer fs = right (fs, [])

-- import foo (? :as f) (? :refer [bar baz]
scanImport :: AST -> Runtime Op
scanImport f@(AList (_:l) _) = case l of
  [] -> left $ Expected "symbol naming module"
  f2@(ASymbol (Nothing,s) _):r ->
    do (r2, alias) <- importAs r
       (r3, refer) <- importRefer r2
       case r3 of
         [] -> right $ OImport (Import s alias refer)
         e:r4 -> left $ Unexpected (show e) -- TODO: better error message. maybe they got the order wrong or misspelled or whatever
  _ -> left $ Expected "non-namespaced symbol naming a module"

-- if condn then else
scanIf :: AST -> Runtime Op
scanIf f@(AList (_:l) _) =
  case l of
    [c,t,e] -> do [c',t',e'] <- mapM scan l
                  right $ OIf c' t' e'
    _ -> left $ Expected "three subexpressions: condition, then, else"

scanList :: AST -> Runtime Op
scanList f@(AList l _) = case l of
  [] -> right $ RT $ RList [] M.empty
  (l1:ls) -> case l1 of
    f2@(ASymbol (_,s) _) -> case s of
      "import" -> scanImport f
      "if" -> scanIf f
      _ -> left $ Unimplemented
    _ -> (RT . (flip RList) M.empty) <$> mapM scan ls
scanList f = left (Expected "List")

scanTuple :: AST -> Runtime Op
scanTuple f@(ATuple t _) = (RT . (flip RTuple) M.empty) <$> (mapM scan t)

scan :: AST -> Runtime Op
scan a = case a of
  ANil         -> return $ RT RNil
  AInt i _     -> return $ RT (RInt i M.empty)
  AFloat f _   -> return $ RT (RFloat f M.empty)
  AChar ch _   -> return $ RT (RChar ch M.empty)
  ASymbol n _  -> return $ RT (RSymbol n M.empty)
  AKeyword k _ -> return $ RT (RKeyword k M.empty)
  AList _ _    -> scanList a
  ATuple _ _   -> scanTuple a
  -- ARecord r -> scanRecord c f
  _ -> left Unimplemented

-- scanListType :: [AST] -> Runtime (Maybe Type)
-- scanListType f = return Nothing

-- scanTypeTag :: AST -> Runtime (Maybe Type)
-- scanTypeTag (AST a p m) = case a of
--   (ASymbol (Name s)) -> return $ Just $ TCon (Tycon s Star)
--   (AList l) -> scanListType l
