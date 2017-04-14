(defalias Txid Int)
(deftype ^[Value] Datom [^Entity e ^Attribute a ^Value v ^Txid t ^Bool added])
(deftype Transaction [^Txid id ^Nanotime time])

(deftype DB)


(deftype Block [^Txid start ^Txid end ^(Vector Datom) datoms])
(deftype BlockIndex)
(deftype ^[Value] BlockLog [^(IntervalTree Txid Block) blocks])





(deftype ViewIndex)
