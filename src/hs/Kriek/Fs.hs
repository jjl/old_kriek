module Kriek.Fs where

import System.FilePath
import System.Posix.Files

lookupClasspath :: FilePath -> [FilePath] -> IO (Maybe FilePath)
lookupClasspath f [] = return Nothing
lookupClasspath f (p:r) =
  do e <- fileExist fp
     if e then return (Just fp)
     else lookupClasspath f r
  where fp = p </> f
