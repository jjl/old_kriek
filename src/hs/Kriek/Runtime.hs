module Kriek.Runtime where

import Control.Monad.State.Strict
import qualified Data.HashMap.Strict as M
import Kriek.Ast
import Kriek.Runtime.Data

findMacro :: Name -> Runtime (Maybe Fn)
findMacro n = gets (M.lookup n . stateMacros)

macroexpand1 :: Form -> Runtime Form
macroexpand1 f@(Form (AList ((Form (ASymbol n) _):rst)) _) =
  do m <- (findMacro n)
     case m of
       Just mac -> mac rst
       _ -> return f
macroexpand1 f = return f

macroexpand :: Form -> Runtime Form
macroexpand f = do f' <- macroexpand1 f
                   case f == f' of
                     True -> case f' of
                       Form (AList (s:r)) p ->
                         do r' <- mapM macroexpand r
                            return $ Form (AList (s:r')) p
                       Form (ATuple l) p ->
                         do l' <- mapM macroexpand l
                            return $ Form (ATuple l') p
                       Form (ARecord l) p ->
                         do l' <- mapM h l
                            return $ Form (ARecord l') p
                       _ -> return f'
                     False -> macroexpand f'
  where h (x,y) = macroexpand y >>= \y' -> return (x, y')

-- eval :: Form RT ->
