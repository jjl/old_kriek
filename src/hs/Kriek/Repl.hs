module Kriek.Repl (read, print, eval, repl) where

import Kriek.AST (Form)
import Kriek.Reader (program)
import Text.Megaparsec (parse, parseErrorPretty)
import Prelude hiding (print, read)

read :: IO String
read = getLine

eval :: String -> String
eval = id

print :: String -> IO ()
print = putStrLn . show

repl :: IO ()
repl = do
    input <- read
    print $ eval input
    repl
