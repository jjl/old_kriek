{-# LANGUAGE Strict #-}
module Kriek.Runtime.Analyser where

import Control.Monad.Reader
import Data.Either (partitionEithers)
import Data.Scientific (Scientific)
import Data.HashMap.Strict

import Kriek.Ast
import Kriek.Ir
import Kriek.Types


import Prelude hiding (read, lookup)

data ErrorId = Unimplemented
             | Expected String

data Error = Error Form ErrorId
           | Errors [Error]

data Analysis = Analysis
  { op :: Op
  , context :: Context
  , pos :: Maybe Position
  , scope :: Scope }

type Result = Either Error Analysis

data Context = Stmt | Expr | Ret

data Value = VNil
           | VInt Integer
           | VFloat Scientific
           | VChar Char
           | VString String
           | VSymbol Name
           | VKeyword Name
           | VList [Analysis]
           | VTuple [Analysis]
           | VRecord [(Analysis, Analysis)]
           
data Op = OValue Value
        | OImport Import
        -- | OIf
        -- | ODef
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

-- fixme
analyseListType :: [Form] -> Runtime (Maybe Type)
analyseListType f = return Nothing

analyseTypeTag :: Form -> Runtime (Maybe Type)
analyseTypeTag (Form a p m) = case a of
  (ASymbol (Name s)) -> return $ Just $ TCon (Tycon s Star)
  (AList l) -> analyseListType l

annotate :: Context -> (Maybe Position) -> Op -> Runtime Result
annotate c p o = do x <- ask
                    return $ Right $ Analysis o c p x

analyseImport :: Form -> Runtime Result
analyseImport f@(Form (AList (_:l)) p _) = case l of
  [] -> return $ Left $ Error f $ Expected "symbol naming module"

analyseMany :: Context -> [Form] -> Runtime (Either Error [Analysis])
analyseMany c fs = do l <- mapM (analyse c) fs
                      let (lefts, rights) = partitionEithers l
                      return $ case lefts of
                        [] -> Right rights
                        _ -> Left $ Errors lefts

analyseList :: Context -> Form -> Runtime Result
analyseList c f@(Form (AList l) p _) = case l of
  [] -> annotate Expr p (OValue $ VList [])
  (l1:ls) -> case l1 of
    f2@(Form (ASymbol (Name s)) p' _) -> case s of
      "import" -> analyseImport f
      "if" -> return $ Left $ Error f2 Unimplemented
      _ -> return $ Left $ Error f2 Unimplemented
    _ -> analyseMany c ls >>= \l' -> case l' of
      Right r -> annotate c p (OValue $ VList r)
      Left e -> return $ Left e

analyseList _ f = return $ Left $ Error f (Expected "List")

analyseNil :: Context -> Form -> Runtime Result
analyseNil c (Form a p _) = annotate c p (OValue VNil)

analyseInt :: Context -> Form -> Runtime Result
analyseInt c (Form (AInt i) p _) = annotate c p (OValue $ VInt i)

analyseFloat :: Context -> Form -> Runtime Result
analyseFloat c (Form (AFloat fl) p _) = annotate c p (OValue $ VFloat fl)

analyseChar :: Context -> Form -> Runtime Result
analyseChar c (Form (AChar ch) p _) = annotate c p (OValue $ VChar ch)

analyseSymbol :: Context -> Form -> Runtime Result
analyseSymbol c (Form (ASymbol n) p _) = annotate c p (OValue $ VSymbol n)

analyseKeyword :: Context -> Form -> Runtime Result
analyseKeyword c (Form (AKeyword n) p _) = annotate c p (OValue $ VKeyword n)

analyseTuple :: Context -> Form -> Runtime Result
analyseTuple c f@(Form (ATuple t) p _) = analyseMany c t >>= \v ->
  case v of
    Left e -> return $ Left e
    Right r -> annotate c p (OValue $ VTuple r)

analyse :: Context -> Form -> Runtime Result
analyse c f@(Form a p _) = case a of
  ANil -> analyseNil c f
  AInt _ -> analyseInt c f
  AFloat _ -> analyseFloat c f
  AChar _ -> analyseChar c f
  ASymbol _ -> analyseSymbol c f
  AKeyword _ -> analyseKeyword c f
  AList _ -> analyseList c f
  ATuple _ -> analyseTuple c f
  -- ARecord _ -> analyseRecord c f
  _ -> return $ Left $ Error f Unimplemented

