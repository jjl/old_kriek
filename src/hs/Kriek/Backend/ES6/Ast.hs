module Kriek.Backend.ES6.Ast where

import Data.List.Utils (replace)

-- FIXME: actually mangle
mangle, demangle :: String -> String
mangle = id
demangle = id

data JS = JSLit String
        | JSComment String
        | JSConst String JS
        | JSFun String [String] [JS]

print :: JS -> String
print (JSLit s) = s
print (JSComment s) = "/* " ++ (replace "*/" "*\\/" s) ++ " */"
