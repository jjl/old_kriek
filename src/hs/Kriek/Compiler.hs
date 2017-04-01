module Kriek.Compiler (compile, compileFile) where

import Kriek.Reader (program)
import Text.Megaparsec (parse, parseErrorPretty)
import Data.Maybe

-- Accepts an optional file path of the file being compiled.
--
-- TODO: Don't use strings
compile :: Maybe FilePath -> String -> String
compile path s = case parse program p s of
                   Left e -> error $ parseErrorPretty e
                   Right x -> show x
  where p = fromMaybe "(unknown)" path

-- Compile file from `src` and store the output in `out`.
compileFile :: FilePath -> FilePath -> IO ()
compileFile src out = do
  contents <- readFile src
  writeFile out $ compile (Just src) contents
