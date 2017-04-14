module Kriek.Repl where

import Kriek.Data
import Kriek.Reader (program)
import Kriek.Runtime
import Kriek.Ir
import Control.Monad.State.Strict hiding (State)
import Text.Megaparsec (parse, parseErrorPretty)
import System.IO (stdout, hFlush)
import Prelude hiding (print, read)

-- Flush string to stdout
flushStr :: String -> IO ()
flushStr str = putStr str >> hFlush stdout

-- String to use for the REPL prompt
prompt :: String
prompt = "kriek> "

read :: IO [AST]
read = do
    flushStr prompt
    input <- getLine
    case parse program "" input of
        Left e -> do
          putStrLn $ parseErrorPretty e
          return []
        Right x -> return x

-- eval :: File -> [Form] -> IO (File, [Form])
-- eval s f = return (s,f) -- runStateT

print :: [AST] -> IO ()
print = putStrLn . show

repl :: Context -> IO ()
repl c = read >>= print >> repl c
