{-# LANGUAGE InstanceSigs #-}

module Monoid where

import Prelude hiding ((<>),mempty,Monoid)
 -- ocultamos algunas cosas del Preludio porque vamos a redefinirlas

------------------------------------------------
-- Monoides
--------------------------------------------------

class Monoid m where
  mempty :: m
  (<>) :: m -> m -> m
{- tal que
  mempty <> x = x
  x <> mempty = x
(x <> y) <> z = x <> (y <> z)
-}

infixr 6 <>  -- precedencia y asociatividad de <>




-- Una instancia de monoide para listas.
instance Monoid [a] where
  mempty = []
  (<>) = (++)





mcat :: Monoid a => [a] -> a
mcat [] = mempty
mcat (x:xs) = x <> mcat xs

--- Listas como monoide libre
monoidfree :: Monoid m => (a -> m) -> [a] -> m
monoidfree f = mcat . map f





{- Esta no es la única instancia de monoide para listas -}
newtype RevList a = RevL { getRevL :: [a] }
              deriving Show

{- Para definir otra instancia, tenemos que definir un nuevo tipo
de datos (RevList).
Junto con el tipo de datos creamos un destructor (getRevL)
(aprovechando la notación para records de Haskell)
El código de más arriba es equivalente a escribir:

newtype RevList a = RevL [a]

getRevL :: RevList a -> [a]
getRevL (RevL xs) = xs
-}

instance Monoid (RevList a) where
  mempty = RevL []
  xs <> ys = RevL $ getRevL ys ++ getRevL xs--

ejRevList = RevL "mundo" <> RevL "hola "






--------------------------------
-- Algunos Monoides
--------------------------------

-- Booleanos
newtype Any = Any {getAny :: Bool}
      deriving Show

instance Monoid Any where
  mempty = Any False
  Any True  <> _  = Any True
  Any False <> x  = x





ejAny1 = Any False <> Any True <> Any True

ejAny2 = mcat [Any False, Any True, Any True]

ejAny3 = mcat $ map Any [False,True,True]

ejAny4 = monoidfree Any [False,True,True]





--------------------------------
newtype All = All {getAll :: Bool}
     deriving Show

instance Monoid All where
  mempty = All True
  All False <> _ = All False
  All True  <> x = x





----------------------------------
-- Instancias con Maybe
----------------------------------
{- data Maybe a = Nothing | Just a -}

newtype First a = First (Maybe a)
           deriving Show

instance Monoid (First a) where
  mempty = First Nothing
  First (Just a) <> _ = First (Just a)
  First Nothing  <> x = x

{- Ejercicio
  Definir un tipo de datos llamado Last que sea isomorfo a Maybe y dar una
  instancia de monoide que devuelva siempre el último valor
-}

----------------------------------
-- Monoide Dual
----------------------------------
{- Dado un monoide podemos
 construir el monoide dual, que será diferente, siempre y cuando
la operación binaria no sea conmutativa.
-}

newtype Dual a = Dual a
     deriving Show

instance Monoid m => Monoid (Dual m) where
  mempty = Dual mempty
  Dual x <> Dual y = Dual (y <> x)

{-
Mediante esta construcción podemos ver, por ejemplo, que la relación
entre listas y RevList, y la de First y Last, es exactamente la misma.

-}
------------------------------------
{- Monoide de endofunciones -}
{- Ejercicio: Definir el monoide de endofunciones -}
newtype Endo a = Endo (a -> a)


------------------------------------
{-
Ejercicio: Mediante una instancia, mostrar que un par de monoides define
un monoide de pares
-}
