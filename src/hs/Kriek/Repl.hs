module Kriek.Repl (repl) where

import Kriek.AST (Form)
import Kriek.Reader (program)
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
        Left e -> error $ parseErrorPretty e
        Right x -> return x

eval :: [Form] -> [Form]
eval = id

print :: [Form] -> IO ()
print = putStrLn . show

repl :: IO ()
repl = read >>= (print . eval) >> repl
