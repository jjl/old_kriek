module Kriek.Repl (repl) where

import Kriek.Ast
import Kriek.Reader (program)
import Kriek.Runtime.Data
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

read :: IO [Form]
read = do
    flushStr prompt
    input <- getLine
    case parse program "" input of
        Left e -> do
          putStrLn $ parseErrorPretty e
          return []
        Right x -> return x

eval :: State -> [Form] -> IO (State, [Form])
eval s f = return (s,f) -- runStateT

print :: [Form] -> IO ()
print = putStrLn . show

repl :: State -> IO ()
repl s = read >>= eval s >>= h
  where h (s2, f) = print f >> repl s2

