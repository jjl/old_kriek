module Kriek.Runtime where

import Control.Monad.Reader
import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as M
import Kriek.Ir
import Kriek.Data

data Scope
data File

type Runtime r = StateT Scope (StateT File IO) r

special :: M.HashMap String (r -> Runtime r)
special = M.fromList s
  where s = [("def", return)]

-- findMacro :: Name -> Runtime (Maybe Fn)
-- findMacro n = gets (M.lookup n . stateMacros)

-- macroexpand1 :: Form -> Runtime Form
-- macroexpand1 f@(Form (AList ((Form (ASymbol n) _ _):rst)) _ _) =
--   do m <- (findMacro n)
--      case m of
--        Just mac -> mac rst
--        _ -> return f
-- macroexpand1 f = return f

-- macroexpand :: Form -> Runtime Form
-- macroexpand f = do f' <- macroexpand1 f
--                    case f == f' of
--                      True -> case f' of
--                        Form (AList (s:r)) p m ->
--                          do r' <- mapM macroexpand r
--                             return $ Form (AList (s:r')) p m
--                        Form (ATuple l) p m ->
--                          do l' <- mapM macroexpand l
--                             return $ Form (ATuple l') p m
--                        Form (ARecord l) p m ->
--                          do l' <- mapM h l
--                             return $ Form (ARecord l') p m
--                        _ -> return f'
--                      False -> macroexpand f'
--   where h (x,y) = macroexpand y >>= \y' -> return (x, y')

-- eval :: IR -> Runtime IR
-- eval = return

-- subscope :: Runtime a -> Runtime a
-- subscope = withStateT recurse
--   where recurse s = Scope (scopeRec s) []
