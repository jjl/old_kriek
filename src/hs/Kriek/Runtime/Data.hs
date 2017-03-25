{-# language MultiParamTypeClasses #-}
module Kriek.Runtime.Data where

import Control.Monad.Reader
import Control.Monad.State.Strict hiding (State)
import Control.Monad.Except
import qualified Data.HashMap.Strict as M
import Kriek.Ast

type Fn = [Form RT] -> Runtime (Form RT)

type Runtime r = StateT State IO r

type Map = M.HashMap Name (AST RT)

data RT = RTFn Fn

instance Show RT where
  show (RTFn _) = "function"

instance Eq RT where
  _ == _ = False

data State = State { stateVals :: Map, stateMacros :: M.HashMap Name Fn }

newState :: State
newState = State M.empty M.empty
