{-# language MultiParamTypeClasses #-}
module Kriek.Ir where

import Control.Monad.Reader
import Control.Monad.State.Strict hiding (State)
import Control.Monad.Except
import qualified Data.HashMap.Strict as M
import Kriek.Data



-- type Runtime a r = ReaderT (Scope a) (StateT (State a) IO) r

-- data RT = RTFn Fn

-- instance Show RT where
--   show (RTFn _) = "function"

-- instance Eq RT where
--   _ == _ = False

-- data IName = IName Name
--            | IRename { rFrom :: Name, rTo ::  Name }

