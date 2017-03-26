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

read :: IO [Form a]
read = do
    flushStr prompt
    input <- getLine
    case parse program "" (input ++ "\n") of
        Left e -> do
          putStrLn $ parseErrorPretty e
          return []
        Right x -> return x

eval :: [Form RT] -> Runtime [Form RT]
eval = return

print :: [Form RT] -> IO ()
print = putStrLn . show

repl :: State -> IO ()
repl s = read >>= h >>= print >> repl s
  where h f = evalStateT (eval f) newState
