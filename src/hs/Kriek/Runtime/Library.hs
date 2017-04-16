{-# language MultiParamTypeClasses #-}
module Kriek.Runtime.Library where

import Kriek.Data
import Data.Scientific
import Control.Applicative
import Control.Monad.Trans.Either
import Control.Monad.Reader
import Control.Monad.State.Strict hiding (State)
import qualified Data.HashMap.Strict as M

toInt :: RT -> Either Error Integer
toInt (RInt i _) = Right i
toInt _ = Left $ Expected "integer"

toFloat :: RT -> Either Error Scientific
toFloat (RFloat f _) = Right f
toFloat _ = Left $ Expected "float"

toNum :: RT -> Either Error Scientific
toNum (RInt i _) = Right $ scientific i (-1)
toNum (RFloat f _) = Right f

-- this is a v1, it doesn't behave right wrt type
-- we should track what type is expected (when we get types...)
plus :: [RT] -> Runtime RT
plus vs = (flip RFloat) M.empty . foldr (+) 0 <$> hoistEither (mapM toNum vs)
