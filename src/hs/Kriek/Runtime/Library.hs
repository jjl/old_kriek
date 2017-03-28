{-# language MultiParamTypeClasses #-}
module Kriek.Runtime.Library where

import Control.Monad.Reader
import Control.Monad.State.Strict hiding (State)
import Control.Monad.Except
import qualified Data.HashMap.Strict as M
import Kriek.Ast
import Kriek.Ir


