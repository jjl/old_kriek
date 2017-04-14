module Kriek.Backend.ES6.Analyser where

import Kriek.Data
import Kriek.Backend.ES6.Ast
import Control.Monad.Trans.Either

data AnalysisCtx

data ES6Error = Unimplemented

type Analyser r = EitherT ES6Error IO r

data Op = Op

aList :: AST -> Scanner Op
aList f@(AList l _) = case l of
  [] -> right $ RT $ RList [] M.empty
  (l1:ls) -> case l1 of
    f2@(ASymbol (_,s) _) -> case s of
      "import" -> scanImport f
      "if" -> scanIf f
      _ -> left $ Unimplemented
    _ -> (RT . (flip RList) M.empty) <$> mapM scanAny ls
aList f = left (Expected "List")

aTuple :: AST -> Scanner Op
aTuple f@(ATuple t _) = (OData . (flip RTuple) M.empty) <$> (mapM scanAny t)

aAny :: AST -> Scanner Op
aAny a = case a of
  ANil       -> right $ RT RNil
  AInt i _     -> right $ RT (RInt i M.empty)
  AFloat f _   -> right $ RT (RFloat f M.empty)
  AChar ch _  -> right $ RT (RChar ch M.empty)
  ASymbol n _ -> right $ RT (RSymbol n M.empty)
  AKeyword k _-> right $ RT (RKeyword k M.empty)
  AList _ _   -> scanList a
  ATuple _ _  -> scanTuple a
  -- ARecord r -> scanRecord c f
  _ -> left $ Unimplemented

analyse :: AST -> IO (Either Error Op)


