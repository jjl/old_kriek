module Kriek.Runtime where

import Control.Monad.Trans.Either
import Control.Monad.State.Strict
import Data.HashMap.Strict as M
import Data.HashSet as S
import Kriek.Scanner
import Kriek.Ir
import Kriek.Data
import Prelude as P

data Scope = Scope { sRec :: HashMap Name Sexp }
newScope = Scope M.empty

data RT

type Runtime = EitherT Error (StateT Context IO)

type Sexp = Either RT (AST Value)

data Macro = Macro ([Form] -> Runtime Form)

data Fn = Fn ([Value] -> Runtime Value)

newtype Global = Global (HashMap (String,String) (AST Value))

newGlobal :: Global
newGlobal = Global M.empty

data Local = Local
  { locModImports :: HashMap String String
  , locDefImports :: HashMap String String
  , locDefs :: HashMap String Value
  , locMacros :: HashMap Name Macro
  , locScope :: Scope}

newLocal :: Local
newLocal = Local M.empty M.empty M.empty M.empty newScope

type Context = (Global, Local)

newContext :: Context
newContext = (newGlobal, newLocal)

importModule :: String -> String -> Context -> Runtime Context
importModule alias name (g,(Local mi di de ma sc)) =
  case (M.lookup alias mi) of
    Just v -> left $ ReboundAlias name v
    _ -> right $ (g,Local (M.insert alias name mi) di de ma sc)

referModule :: String -> [String] -> Context -> Runtime Context
referModule mod names (g,(Local mi di de ma sc)) =
  do ns <- mapM h names
     let di' = di `mappend` (mconcat ns)
     right (g, Local mi di' de ma sc)
  where h k = case M.lookup k di of
                Just m -> left $ ReboundAlias k m
                _ -> right $ M.singleton k mod

getLoc :: Runtime Local
getLoc = gets snd
getGlo :: Runtime Global
getGlo = gets fst

special :: HashSet String
special = S.fromList ["def"]

findMacro :: Name -> Runtime (Maybe Macro)
findMacro n = (M.lookup n . locMacros) <$> getLoc

macroexpand1 :: Form -> Runtime Form
macroexpand1 f@(Form (AList ((Form (ASymbol n) _ _):rst)) _ _) =
  do m <- (findMacro n)
     case m of
       Just (Macro mac) -> mac rst
       _ -> return f
macroexpand1 f = return f

macroexpand :: Form -> Runtime Form
macroexpand f = do f' <- macroexpand1 f
                   case f == f' of
                     True -> case f' of
                       Form (AList (s:r)) p m ->
                         do r' <- mapM macroexpand r
                            return $ Form (AList (s:r')) p m
                       Form (ATuple l) p m ->
                         do l' <- mapM macroexpand l
                            return $ Form (ATuple l') p m
                       Form (ARecord l) p m ->
                         do l' <- mapM h l
                            return $ Form (ARecord l') p m
                       _ -> return f'
                     False -> macroexpand f'
  where h (x,y) = macroexpand y >>= \y' -> return (x, y')

evalValue :: Op -> Runtime Value
evalValue (OValue v) = left Unimplemented

defaultImportAlias m = m -- FIXME: take the bit after the last dot

evalImport :: Op -> Runtime Value
evalImport (OImport (Import m a r)) =
  get >>= importModule m a' >>= referModule m r >>= put >> right ANil
  where a' = maybe m id a

evalIf :: Op -> Runtime Value
evalIf (OIf c t e) = left Unimplemented

evalDef :: Op -> Runtime Value
evalDef (ODef n v) = left Unimplemented

eval :: Op -> Runtime Value
eval o = case o of
  OValue _  -> evalValue o
  OImport _ -> evalImport o
  OIf _ _ _ -> evalIf o
  ODef _ _  -> evalDef o

-- subscope :: Runtime a -> Runtime a
-- subscope = withStateT recurse
--   where recurse s = Scope (scopeRec s) []
