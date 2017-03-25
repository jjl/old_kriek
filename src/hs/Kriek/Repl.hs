module Kriek.Repl (repl) where

import Kriek.Ast
import Kriek.Reader (program)
import Kriek.Runtime.Library
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
    case parse program "" input of
        Left e -> error $ parseErrorPretty e
        Right x -> return x

eval :: [Form RT] -> [Form RT]
eval = id

print :: [Form RT] -> IO ()
print = putStrLn . show

repl :: IO ()
repl = read >>= (print . eval) >> repl
