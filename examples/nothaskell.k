double? :: (maybe int) -> (maybe int) ; inferred type!
defn double? [a]
  match a
    nil a
    int (a * 2)

double? :: list (maybe int) -> list (maybe int) ; inferred type!
defn double?-list [a]
  map double? a

defcontract Monad [m :: * -> *, a]
  return :: a -> m a
  (>>=) :: m a -> (a -> m a) -> m a
  