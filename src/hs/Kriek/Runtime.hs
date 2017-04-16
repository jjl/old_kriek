{-# language DuplicateRecordFields, LiberalTypeSynonyms, RankNTypes #-}
module Kriek.Runtime where

import Control.Applicative ((<|>))
import Control.Lens ( makeLenses
                    , (^.), (.~), (%~), (?~), (?=), (%=)
                    , _1, _2, at, ix, Lens'(..))
import Control.Monad.Reader
import Control.Monad.Trans.Either
import Control.Monad.State.Strict
import Data.Maybe
import Data.List.Split (splitOn)
import Data.HashMap.Strict as M
import Data.HashSet as S
import Kriek.Scanner
import Kriek.Ir
import Kriek.Data
import System.FilePath (takeFileName)
import Prelude as P

type ALens s t a b = forall f. Applicative f => (a -> f b) -> s -> f t 
type ALens' s a = ALens s s a a

glo :: Lens' Context Global
glo = ctxGlobal

loc :: Lens' Context Local
loc = ctxLocal

maybeNamedModule :: String -> Lens' Context (Maybe Module)
maybeNamedModule s = glo . modules . at s

namedModule :: String -> ALens' Context Module
namedModule s = glo . modules . ix s

lookupModule :: String -> Runtime (Maybe Module)
lookupModule m = (^. maybeNamedModule m) <$> get

lookupQname :: String -> String -> Runtime (Maybe RT)
lookupQname m n = (>>= M.lookup n . (^. defs)) <$> lookupModule m

curname :: Lens' Context String
curname = loc . cur_module

current :: Context -> ALens' Context Module
current = namedModule . (^. curname)

namedRefer :: String -> Context -> ALens' Context (Maybe String)
namedRefer n c = current c . refers . at n

namedImport :: String -> Context -> ALens' Context (Maybe String)
namedImport n c = current c . imports . at n

lookupLexical :: String -> Runtime (Maybe RT)
lookupLexical n = do s <- ask
                     return $ (s ^. nonrec . at n) <|> (s ^. recursive . at n)

lookupLocal :: String -> Runtime (Maybe RT)
lookupLocal n = do c <- get
                   return $ M.lookup n (c ^. current c . defs)

lookupRef :: String -> String -> Runtime (Maybe RT)
lookupRef m n = do (^. namedModule m . defs . at n) <$> get

findMacro :: (Maybe String,String) -> Runtime (Maybe (Fn Runtime RT))
findMacro (ns,n) = do c <- get
                      let d = c ^. maybe (current c) namedModule ns . defs . at n
                      return $ case d of
                        Just (RMacro f _) -> Just f
                        _ -> Nothing

changeModule :: String -> Runtime ()
changeModule m = curname %= const m

defineValue :: String -> RT -> Runtime ()
defineValue s v = do c <- get
                     put $ (current c . defs . at s) ?~ v $ c
                     
-- FIXME: actually go and read the module
importModule :: String -> String -> Runtime ()
importModule alias name =
  do c <- get
     case c ^. (namedImport alias c) of
       Just _ -> left $ ReboundAlias name alias
       _ -> do _ <- changeModule name
               -- FIXME: read the module
               changeModule (c ^. curname)

referModule :: String -> [String] -> Runtime ()
referModule mod names =
  do c <- get
     ns <- mapM (h c) names
     current c . refers %= (`mappend` (mconcat ns))
  where h c k = do left Unimplemented

special :: HashSet String
special = S.fromList ["def"]

isSpecial :: (Maybe String, String) -> Bool
isSpecial (Just _,_) = False
isSpecial (_,n) = S.member n special

-- macroexpand1 :: RT -> Runtime RT
-- macroexpand1 f@(RList ((RT (RSymbol n _)):rst) _) =
--   if isSpecial n then return f
--   else do c <- get
--           m <- findMacro n
--           case m of
--             Just (Fn f) -> case mapM ropToRt rst of
--                              Right r -> f r
--                              Left l -> left $ Unexpected (show l)
--             _ -> return f
-- macroexpand1 f = return f

-- macroexpand :: RT -> Runtime RT
-- macroexpand f = do f' <- macroexpand1 f
--                    if f == f' then
--                      case f' of
--                        RList (s:r) m ->
--                          do case mapM ropToRt r of
--                               Right r' -> do r''<- mapM macroexpand r'
--                                              return $ RList (s:(fmap RT r'')) m
--                               Left l -> left $ Unexpected (show l)
--                        RTuple l m ->
--                          do case mapM ropToRt l of
--                               Right r -> do r'<- mapM macroexpand r
--                                             return $ RTuple (fmap RT r') m
--                               Left l -> left $ Unexpected (show l)
--                        RRecord l m ->
--                          do case mapM h2 l of
--                               Right r -> do r'<- mapM h r
--                                             return $ RRecord r' m
--                               Left l -> left $ Unexpected (show l)
--                        _ -> return f'
--                    else macroexpand f'
--   where h (x,y) = macroexpand y >>= \y' -> return (RT x, RT y')
--         h2 (x,y) = do x' <- ropToRt x; y' <- ropToRt y; return (x',y')

evalValue :: RT -> Runtime RT
evalValue f = case f of
    RList    l m -> do l' <- mapM eval l
                       return $ RList (fmap RT l') m
    RTuple   l m -> do l' <- mapM eval l
                       return $ RTuple (fmap RT l') m
    RRecord  l m -> do l' <- mapM h l
                       return $ RRecord l' m
    _ -> return f
  where h (l,r) = do l' <- eval l
                     r' <- eval r
                     return (RT l',RT r')
                 
evalIf :: Op -> Op -> Op -> Runtime RT
evalIf c t e =
  do c' <- eval c
     case c' of
       RBool True  _ -> eval t
       RBool False _ -> eval e

evalDo :: [Op] -> Runtime RT
evalDo [] = return RNil
evalDo (o:os) = eval o >> evalDo os

evalDef :: String -> (Maybe String) -> Op -> Runtime RT
evalDef n d v = eval v >>= defineValue n >> return RNil

-- evalFun :: Maybe String -> [Op] -> [Op] -> Runtime RT
-- evalFun n p b = 

evalImport :: Import -> Runtime RT
evalImport (Import m a r) =
  importModule m a' >> referModule m r >> right RNil
  where a' = maybe (takeFileName m) id a

evalLet :: [(String, Bool, Op)] -> [Op] -> Runtime RT
evalLet [] e = eval (ODo e)
evalLet ((n,True,o):bs) e =
  do v <- eval o
     local (recursive . at n ?~ v) (evalLet bs e)

evalLexical  :: String -> Runtime RT
evalLexical n = do l <- lookupLexical n
                   case l of
                     Just n' -> return n'
                     _ -> left $ UndefinedLexical n

evalLocal :: String -> Runtime RT
evalLocal n = do v <- lookupLocal n
                 case v of
                   Just v' -> return v'
                   _ -> left $ UndefinedLocal n

evalRef :: String -> String -> Runtime RT
evalRef m n = do r <- lookupRef m n
                 case r of
                   Just v' -> return v'
                   _ -> left $ UndefinedRef m n

evalInvoke :: (Fn Runtime RT) -> [Op] -> Runtime RT
evalInvoke (Builtin f) os = mapM eval os >>= f

eval :: Op -> Runtime RT
eval o = case o of
  RT v       -> evalValue v
  OIf i t e  -> evalIf i t e
  ODo es     -> evalDo es
  ODef n d v -> evalDef n d v
  -- OFun n p b -> evalFun n p b
  OImport i  -> evalImport i
  OLet b e   -> evalLet b e
  OLexical n -> evalLexical n
  OLocal n   -> evalLocal n
  ORef m n   -> evalRef m n
  OInvoke f a -> evalInvoke f a

-- runRuntime :: Runtime RT -> Scope -> Context -> IO (Either Error (RT,Context))
-- runRuntime a s c = runStateT (runReaderT (runEitherT a) s) c

