{-# LANGUAGE TypeOperators  #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE InstanceSigs #-}


module Generics where

import Functors


--------------------------------------------------------------
-- Algebra de Funtores
--------------------------------------------------------------

-- Uno de los funtores más basicos es el funtor identidad

data Id x = Id x

instance Functor Id where
  fmap f (Id x) = Id (f x)








-- Dado un tipo cualquiera podemos construir un funtor constante -}

data K a x = K a

instance Functor (K a) where
  fmap _ (K a) = K a








-- También tenemos que el producto de funtores es un funtor
infixr 6 :*:

data (f :*: g) a = FunProd (f a) (g a)

instance (Functor f, Functor g) => Functor (f :*: g) where
  fmap :: (a -> b) -> (f :*: g) a -> (f :*: g) b
  fmap h (FunProd fx gx) = FunProd (fmap h fx) (fmap h gx)







{- Suma de funtores (coproducto de funtores) -}
infixr 5 :+:

data (f :+: g) a = Inl (f a) | Inr (g a)

instance (Functor f, Functor g) => Functor (f :+: g) where
  fmap :: (a -> b) -> (f :+: g) a -> (f :+: g) b
  fmap h (Inl fa) = Inl (fmap h fa)
  fmap h (Inr ga) = Inr (fmap h ga)









{- La composición de funtores es un funtor -}
infixr 7 :.:

data (f :.: g) a = FunComp (f (g a))

instance (Functor f, Functor g) => Functor (f :.: g) where
   fmap :: (a -> b) -> (f :.: g) a -> (f :.: g) b
   fmap h (FunComp fga) = FunComp (fmap (fmap h) fga)






{-
  Con estos ingredientes (Id, K, :*:, :+:, :.:) podemos representar
  una gran familia de  tipos de datos
-}


{- Ejemplo:

data Algunos b a = Nada b
                | Uno a
                | Dos a a
                | Tres a a a
-}
type AlgunosG b = K b
                :+: Id
                :+: Id :*: Id
                :+: Id :*: Id :*: Id
 -- K b :+: ( Id :+: ( Id :*: Id) :+: ( ...))
 -- Algunos G b a = K b :+: ( Id :+: ( Id :*: Id) :+: ( ...)) a
  --  K b a
  -- Id a
  -- (Id :*: Id) a = Id a , Id a
  -- ...

{-
El tipo Algunos y el tipo AlgunosG son isomorfos:
 podemos convertir de uno a otro sin perder información
-}

nada :: b -> AlgunosG b a
nada b = Inl $ K b

uno :: a -> AlgunosG b a
uno a = Inr $ Inl $ Id a

dos :: a -> a -> AlgunosG b a
dos a1 a2 = Inr $ Inr $ Inl $ FunProd (Id a1) (Id a2)

tres :: a -> a -> a -> AlgunosG b a
tres a1 a2 a3 = Inr $ Inr $ Inr $
                FunProd (Id a1)
                        (FunProd (Id a2) (Id a3))




toG :: Algunos b a -> AlgunosG b a
toG (Nada b) = nada b
toG (Uno a) = uno a
toG (Dos a1 a2) = dos a1 a2
toG (Tres a1 a2 a3) = tres a1 a2 a3

fromG :: AlgunosG b a -> Algunos b a
fromG (Inl (K b)) = Nada b
fromG (Inr (Inl (Id a))) = Uno a
fromG (Inr (Inr (Inl (FunProd (Id a1) (Id a2))))) = Dos a1 a2
fromG (Inr (Inr (Inr (FunProd (Id a1)
                    (FunProd (Id a2) (Id a3)))))) = Tres a1 a2 a3


------------------------------------
{-
Podemos crear una clase para los tipo que podemos darle una representación
estructural.

-}

class Generic f where
  type Rep f :: * -> *
  toRep :: f a -> Rep f a
  fromRep :: Rep f a -> f a








instance Generic (Algunos b) where
  type Rep (Algunos b) = AlgunosG b

  toRep = toG
  fromRep = fromG



-- Otro ejemplo

--data Pair a = Pair a a

instance Generic Pair where
  type Rep Pair = Id :*: Id

  toRep (Pair x y) = FunProd (Id x) (Id y)
  fromRep (FunProd (Id x) (Id y)) = Pair x y




{-
EJERCICIO
  Dar la instancia de
  Generic para el tipo
    data Maybe a = Nothing | Just a

-}














  {- Mediante esta representación podemos definir
    funciones genéricas. Por ejemplo, si el funtor es un contenedor
    de números, podemos definir una función que los sume:
  -}

class GSum f where
  gsum :: Num a => f a -> a











instance GSum Id where
  gsum :: Num a => Id a -> a
  gsum (Id a) = a

instance GSum (K b) where
  gsum :: Num a => K b a -> a
  gsum (K _) = 0

instance (GSum f, GSum g) => GSum (f :+: g) where
  gsum :: Num a => (f :+: g) a -> a
  gsum (Inl fa) = gsum fa
  gsum (Inr ga) = gsum ga

instance (GSum f, GSum g) => GSum (f :*: g) where
  gsum :: Num a => (f :*: g) a -> a
  gsum (FunProd fa ga) = gsum fa + gsum ga

algsum :: Algunos b Integer -> Integer
algsum = gsum . toG



sum :: (Generic f, GSum (Rep f), Num a) => f a -> a
sum = gsum . toRep

--------------------------------------------------
{-
Generalizar la suma a cualquier monoide, de manera de poder definir
una función

crush :: (Generic f, ..., Monoid m) => f m -> m

-}

--------------------------------------------------
{- Tipos Recursivos -}

newtype Rec f a = Rec (f a)
  deriving Functor

instance (GSum f) => GSum (Rec f) where
  gsum :: Num a => Rec f a -> a
  gsum (Rec t) = gsum t

instance Generic Bin where
    type Rep Bin = Id :+: Rec Bin :*: Rec Bin

    toRep (Leaf a) = Inl (Id a)
    toRep (Node l r) = Inr $ FunProd (Rec l) (Rec r)

    fromRep (Inl (Id a)) = Leaf a
    fromRep (Inr (FunProd (Rec l) (Rec r))) = Node l r

test1 = Node (Node (Leaf 4) (Leaf 3)) (Leaf 2)

instance GSum Bin where
  gsum = gsum . toRep

--------------------------------------------------
{- Ejemplo: representación genérica del funtor lista -}

{-
Las listas están dadas por la siguiente definición:
  data [a] = [] | a : [a]
-}

instance Generic [] where
  type Rep [] = K () :+: Id :*: []

  toRep [] = Inl (K ())
  toRep (x:xs) = Inr $ FunProd (Id x) xs

  fromRep (Inl _) = []
  fromRep (Inr (FunProd (Id x) xs)) = x : xs

instance GSum [] where
  gsum = gsum . toRep

{- EJERCICIO: Dar la instancia de Generic del tipo
   data Tree a = Empty | Branch (Tree a) a (Tree a)

-}


{- EJERCICIO: Extender la función crush genérica a tipos recursivos
-}


-- Probarla con diferentes monoides: multiplicar
-- todos los enteros en una estructura, chequear que todos
-- los booleanos contenidos son True, que existe alguno True,
-- sumando todos los números, etc.


--------------------------------------------------
{- Probemos ahora de obtener una representación para
 árboles generales

data GenTree a = Gen a [GenTree a]
 -}

instance Generic GenTree where
   type Rep GenTree = Id :*: [] :.: GenTree

   toRep (Gen a xs) = FunProd (Id a) (FunComp xs)
   fromRep (FunProd (Id a) (FunComp xs)) = Gen a xs

instance (Functor f, GSum f, GSum g) => GSum (f :.: g) where
  gsum (FunComp fgx) = gsum (fmap gsum fgx)

instance GSum GenTree where
    gsum = gsum . toRep

{- Ejercicio: extender la función crush genérica a composición de funtores de
 manera de poder utilizarla sobre árboles generales.
 -}

{- El GHC implementa Generics (Magalhães 2010) que automatiza la derivación
   de la representación genérica.

   El enfoque categórico a la programación genérica se puede ver en el
   capítulo 2 de (Bird & de Moor 1997).
-}
