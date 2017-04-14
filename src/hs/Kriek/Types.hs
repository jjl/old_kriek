module Kriek.Types where

import Data.List (nub, (\\), intersect, union, partition)
import Control.Monad (msum) 

-- Bootstrapped from Typing Haskell In Haskell (THIH)
-- http://web.cecs.pdx.edu/~mpj/thih/TypingHaskellInHaskell.html


type Subst = [(Tyvar, Type)]

type Id = String

enumId :: Int -> Id
enumId n = "enumerated_" ++ (show n)

data Kind = Star
          | KArrow Kind Kind
          deriving Eq

data Type = TVar Tyvar
          | TCon Tycon
          | TSynonym String
          | TArrow Type Type
          | TUnion [Type]
          | TGen Int
          deriving Eq

data Tyvar = Tyvar Id Kind
           deriving Eq

data Tycon = Tycon Id Kind
           deriving Eq

class Types t where
  apply :: Subst -> t -> t
  tv :: t -> [Tyvar]

instance Types Type where
  apply s (TVar u) = case lookup u s of
                       Just t -> t
                       Nothing -> TVar u
  apply s (TArrow l r) = TArrow (apply s l) (apply s r)
  apply s t = t

  tv (TVar u) = [u]
  tv (TArrow l r) = tv l `union` tv r
  tv t = []

instance Types a => Types [a] where
  apply s = map (apply s)
  tv      = nub . concat . map tv

class HasKind t where
  kind :: t -> Kind

instance HasKind Tyvar where
  kind (Tyvar v k) = k

instance HasKind Tycon where
  kind (Tycon v k) = k

instance HasKind Type where
  kind (TCon tc) = kind tc
  kind (TVar u)  = kind u
  kind (TArrow t _) = case (kind t) of
                     (KArrow _ k) -> k

(+->)      :: Tyvar -> Type -> Subst
u +-> t     = [(u, t)]

infixr 4 @@
(@@)       :: Subst -> Subst -> Subst
s1 @@ s2    = [ (u, apply s1 t) | (u,t) <- s2 ] ++ s1

merge      :: Monad m => Subst -> Subst -> m Subst
merge s1 s2 = if agree then return (s1++s2) else fail "merge fails"
 where agree = all (\v -> apply s1 (TVar v) == apply s2 (TVar v))
                   (map fst s1 `intersect` map fst s2)

-- mgu     :: Monad m => Type -> Type -> m Subst
mgu (TArrow l r) (TArrow l' r')
  = do s1 <- mgu l l'
       s2 <- mgu (apply s1 r) (apply s1 r')
       return (s2 @@ s1)
mgu (TVar u) t        = varBind u t
mgu t (TVar u)        = varBind u t
mgu (TCon tc1) (TCon tc2)
           | tc1==tc2 = return []
mgu t1 t2             = fail "types do not unify"

varBind :: Monad m => Tyvar -> Type -> m Subst
varBind u t | t == TVar u      = return []
            | u `elem` tv t    = fail "occurs check fails"
            | kind u /= kind t = fail "kinds do not match"
            | otherwise        = return (u +-> t)

match :: Monad m => Type -> Type -> m Subst
match (TArrow l r) (TArrow l' r') =
  do sl <- match l l'
     sr <- match r r'
     merge sl sr
match (TVar u)   t | kind u == kind t = return (u +-> t)
match (TCon tc1) (TCon tc2)
         | tc1==tc2 = return []
match t1 t2 = fail "types do not match"

data Qual t = [Pred] :=> t
              deriving Eq

data Pred   = IsIn Id Type
              deriving Eq

tNil  = TCon (Tycon "nil" Star)
tChar  = TCon (Tycon "char" Star)
tInt8  = TCon (Tycon "int8" Star)
tInt16 = TCon (Tycon "int16" Star)
tInt32 = TCon (Tycon "int32" Star)
tInt64 = TCon (Tycon "int64" Star)
tByte8  = TCon (Tycon "byte8" Star)
tByte16 = TCon (Tycon "byte16" Star)
tByte32 = TCon (Tycon "byte32" Star)
tByte64 = TCon (Tycon "byte64" Star)
tFloat  = TCon (Tycon "float" Star)
tDouble  = TCon (Tycon "double" Star)
tString  = TCon (Tycon "string" Star)
tList = TCon (Tycon "List" (KArrow Star Star))
tArrow = TCon (Tycon "->" (KArrow Star (KArrow Star Star)))

infixr 4 ==>
(==>)  :: Type -> Type -> Type
(==>) a = TArrow (TArrow tArrow a)

list :: Type -> Type
list = TArrow tList
