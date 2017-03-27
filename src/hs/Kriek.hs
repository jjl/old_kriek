{-# LANGUAGE DeriveDataTypeable #-}
module Kriek where

import qualified Kriek.Compiler as C
import qualified Kriek.Repl as R
import Kriek.Runtime.Data (newState)
import System.Console.CmdArgs

data KriekCmd = Repl
              | Compile { src :: FilePath, out :: FilePath }
              deriving (Data, Typeable)

repl :: KriekCmd
repl = Repl

compile :: KriekCmd
compile = Compile
  { src = def &= argPos 0 &= typ "SRC"
  , out = def &= argPos 1 &= typ "OUT"}

main :: IO ()
main = do
  args <- cmdArgs (modes [repl, compile])
  case args of
    Repl -> R.repl newState
    Compile src out -> C.compileFile src out
