{-# LANGUAGE DeriveDataTypeable #-}
module Kriek where

import qualified Kriek.Compiler as C
import qualified Kriek.Repl as R
import Kriek.Runtime (newContext)
import Kriek.Ir
import System.Console.CmdArgs.Implicit

data KriekCmd = Repl
              | Compile { src :: FilePath, out :: FilePath }
              deriving (Data, Typeable)

repl :: KriekCmd
repl = Repl

compile :: KriekCmd
compile = Compile
  { src = def &= argPos 0 &= typ "SRC"
  , out = def &= argPos 1 &= typ "OUT"}


mode = cmdArgsMode $ modes [repl, compile &= auto]
  &= program "kriek"
  &= summary "Kriek v0.1"
  &= help "A statically typed lisp-like language"

main :: IO ()
main = do
  args <- cmdArgsRun mode
  case args of
    Repl -> R.repl newContext
    Compile src out -> C.compileFile src out
