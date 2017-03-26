module Kriek.Runtime where

import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as M
import Kriek.Ast
import Kriek.Runtime.Data

-- lookupMacro :: Env -> Name -> Maybe Form

findMacro :: Name -> Runtime (Maybe Fn)
findMacro n = gets (M.lookup n . stateMacros)
-- we evaluate a data

macroexpand1 :: Form RT -> Runtime (Form RT)
macroexpand1 f@(Form (KList ((Form (KSymbol n) _):rst)) _) =
  do m <- (findMacro n)
     case m of
       Just mac -> mac rst
       _ -> return f
macroexpand1 f = return f

macroexpand :: Form RT -> Runtime (Form RT)
macroexpand f = do f' <- macroexpand1 f
                   case f == f' of
                     True -> case f' of
                       Form (KList (s:r)) p ->
                         do r' <- mapM macroexpand r
                            return $ Form (KList (s:r')) p
                       Form (KTuple l) p ->
                         do l' <- mapM macroexpand l
                            return $ Form (KTuple l') p
                       Form (KRecord l) p ->
                         do l' <- mapM h l
                            return $ Form (KRecord l') p
                       _ -> return f'
                     False -> macroexpand f'
  where h (x,y) = macroexpand y >>= \y' -> return (x, y')

