(import (kriek.ast as a))

(defunion Context
  Statement Expr Ret)

(defunion Op
  If
  Def
  Fn
  Do [^a/AST exprs]
  )



(defn analyse-list
(defrecord Analysis
  [^Context :context ^Op :op])

(defn fac ^Int [^Int v]
  (if (< n 2)
    1
    (* n (fac (dec n)))))
    
{:factorial fac}
