{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}

module Functors where

import Prelude hiding (sum, map)

  -- ------------------------------------------------
  -- -- FUNTORES
  -- ------------------------------------------------
  {-
  Todo lenguaje funcional tiene listas y la función map
  -}
map :: (a -> b) -> [a] -> [b]
map f []     = []
map f (x:xs) = f x : map f xs





{-
  Podemos definir funciones similares para
    varios tipos de datos
  -}

data Bin a = Leaf a | Node (Bin a) (Bin a)
         deriving Show

binmap :: (a -> b) -> Bin a -> Bin b
binmap f (Leaf a) = Leaf (f a)
binmap f (Node l r) = Node (binmap f l) (binmap f r)





data Tree a = Empty | Branch (Tree a) a (Tree a)
         deriving Show

treemap :: (a -> b) -> Tree a -> Tree b
treemap f Empty = Empty
treemap f (Branch l x  r) = Branch (treemap f l)
                                   (f x)
                                   (treemap f r)






data GenTree a = Gen a [GenTree a]
         deriving Show

gentreemap :: (a -> b) -> GenTree a -> GenTree b
gentreemap f (Gen x xs) = Gen (f x) (map (gentreemap f) xs)


  {-
  Notar que en todos los casos son tipos parametrizados
  por un tipo a (tanto listas, como Bin, Tree,
  y GenTree tienen kind * -> *)

  ¿Cómo definir una generalización de map?





  Intuitivamente, podremos generalizar map a un
  constructor de tipos f, si podemos proveer una función

  fmap :: (a -> b) -> f a -> f b

  que aplique el argumento a todos los elementos de a
  “almacenados” en f a.




  Por lo que podemos definir la siguiente clase de tipos

  class Functor f where
    fmap :: (a -> b) -> f a -> f b

  - f tiene kind * -> *

  Podemos definir las siguientes instancias:

  instance Functor [] where
    fmap = map

  (instancia ya provista en el Preludio)
  -}


instance Functor Bin where
     fmap = binmap

instance Functor Tree where
     fmap = treemap

instance Functor GenTree where
     fmap = gentreemap







  -- ̣¿Qué pasa con el siguiente tipo de datos?
  --   (que también tiene kind * -> *)

newtype Func a = Func (a -> a)

funcmap :: (a -> b) -> Func a -> Func b
funcmap g (Func h) = Func id




{-
  Los constructores de tipos que poseen una función fmap con
  “buen comportamiento” son funtores.

  El “buen comportamiento” queda especificado por las
  siguientes ecuaciones:

           fmap id = id
   fmap f . fmap g = fmap (f . g )

  o equivalentemente (por extensionalidad)
          fmap id x = x
   fmap f (fmap g x) = fmap (f . g) x






La clase Functor
================

  Definimos los funtores como

   class Functor f where
     fmap :: (a -> b) -> f a -> f b

  tal que
           fmap id x = x
   fmap f (fmap g x) = fmap (f . g) x

  Pueden verificar que estas ecuaciones se cumplen para
  las instancias dadas de listas, Bin, Tree y GenTree.





  - El compilador no chequea que se cumplan las ecuaciones.
    Es responsabilidad del programador de las instancias chequearlo.





 Extensión DeriveFunctor
 =======================

  En las versiones modernas de GHC se puede usar la extensión
  DeriveFunctor, y escribir, por ejemplo:
  data T a = E | N (T a) a (T a) deriving Functor
  Para obtener automáticamente la instancia de la clase Functor.

  -  Algunas formas de habilitar extensiones:
   - Pasar el parámetro -XDeriveFunctor en la línea de comandos
   - Escribir {-# LANGUAGE DeriveFunctor #-} en el archivo
     fuente
   - Escribir :set -XDeriveFunctor en el entorno
     interactivo (ghci)





La funcion fmap como operador infijo
====================================

  Hay un operador que nos permite escribir en forma infija la operación fmap.
  Es decir,
   <$> = fmap

  Por ejemplo:

  *> succ <$> [1..5]
   [2,3,4,5,6]
-}






-- ----------------------------------------------------
-- -- EJERCICIOS de Funtores
-- ----------------------------------------------------

-- -- Dar instancias de Funtor para los siguientes tipos de datos
-- -- (sin usar derive Functor)

data Pair a = Pair a a
         deriving Show

data Algunos b a = Nada b | Uno a | Dos a a | Tres a a a
         deriving Show

  -- -- Los funtores son más generales que simples contenedores de datos

newtype C r a = C ((a -> r) -> r)

  -- -- ¿Por qué la siguiente instancia (que está bien tipada)
  -- -- no define un funtor?

data Br b a = B b (a, a)

instance Functor (Br b) where
   fmap f (B x (y, z )) = B x (f z , f y)
