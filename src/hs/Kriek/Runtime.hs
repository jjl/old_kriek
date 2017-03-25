module Kriek.Runtime where

import Control.Monad.Reader
import Control.Monad.State.Strict
import Control.Monad.Except
import qualified Data.HashMap.Strict as M
import Kriek.Ast
import Kriek.Runtime.Library

-- lookupMacro :: Env -> Name -> Maybe Form

findMacro :: Name -> Runtime (Maybe Fn)
findMacro n = gets (M.lookup n . stateMacros)
-- we evaluate a data 

macroexpand1 :: Form RT -> Runtime (Form RT)
macroexpand1 f@(Form (KList ((Form (KSymbol n) _ _):rst)) _ _) =
  do m <- (findMacro n)
     case m of
       Just mac -> mac rst
       _ -> return f
macroexpand1 f = return f

-- macroexpand :: Form RT -> Runtime (Form RT)
-- macroexpand f = do f' <- macroexpand1 f
--                    case f == f' of
--                      True -> case f' of
--                        Form (KList (s:r)) p m ->
--                          do r' <- mapM macroexpand r
--                             return $ Form (KList (s:r')) p m
--                        Form (KTuple l) p m ->
--                          do l' <- mapM macroexpand l
--                             return $ Form (KTuple l') p m
--                        Form (KRecord l) p m ->
--                          do l' <- mapM h (x,y')) l
--                             return $ Form (KRecord l') p m
--                        _ -> return f'
--                      False -> macroexpand f'
--   where h (x,y) = y

