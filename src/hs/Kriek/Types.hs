module Kriek.Types where

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

tUnit  = TCon (Tycon "()" Star)
tChar  = TCon (Tycon "Char" Star)
tInt8  = TCon (Tycon "Int8" Star)
tInt16 = TCon (Tycon "Int16" Star)
tInt32 = TCon (Tycon "Int32" Star)
tInt64 = TCon (Tycon "Int64" Star)
tWord8  = TCon (Tycon "Word8" Star)
tWord16 = TCon (Tycon "Word16" Star)
tWord32 = TCon (Tycon "Word32" Star)
tWord64 = TCon (Tycon "Word64" Star)
tFloat  = TCon (Tycon "Float" Star)
tDouble  = TCon (Tycon "Double" Star)
tString  = TCon (Tycon "String" Star)
tList = TCon (Tycon "List" (KArrow Star Star))
tArrow = TCon (Tycon "->" (KArrow Star (KArrow Star Star)))

infixr 4 ==>
(==>)  :: Type -> Type -> Type
(==>) a = TArrow (TArrow tArrow a)

list :: Type -> Type
list = TArrow tList
