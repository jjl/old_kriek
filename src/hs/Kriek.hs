module Kriek where

import Kriek.Repl (repl)
import Kriek.Runtime.Data

main :: IO ()
main = repl newState
