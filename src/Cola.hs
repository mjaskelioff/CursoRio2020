{-# LANGUAGE InstanceSigs #-}

module Cola where

{-
¿Qué es una cola?
 Es una estructura a la cual:
  - Podemos agregar elementos
  - Podemos obtener el primer elemento
  - Podemos quitar el primer elemento
  - Podemos preguntar si está vacía
  - Existe una relación entre el orden en que se agregan
    elementos y se sacan (FIFO).

Esta descripción es abstracta porque refleja el comportamiento
 y no la implementación.
-}






class Cola p where
   vacia :: p a
   esVacia :: p a -> Bool
   poner :: a -> p a -> p a
   sacar :: p a -> p a
   primero :: p a -> a


{- El comportamiento puede ser especificado mediante ecuaciones:
   vacia empty = True
   vacia (poner x p) = False
   sacar (poner x empty) = empty
   sacar (poner x (poner y p)) = poner x (sacar (poner y p))
   primero (poner x empty) = x
   primero (poner x (poner y p)) = primero (poner y p)
-}


-- Una implementación trivial (e ineficiente) es:
instance Cola [] where
  vacia = []
  esVacia = null
  poner x xs = xs++[x]
  sacar = tail
  primero = head







-- Otra implementación mas eficiente
data Queue a = Q [a] [a]
        deriving Show

-- La idea es que
-- Q xs ys  representa el orden
-- xs + reverse ys

listar :: Queue a -> [a]
listar (Q xs ys) = xs ++ reverse ys



-- Invariante: Si xs es vacía, entonces ys también.

-- Para mantener la invariante hacemos
-- un constructor inteligente
mkQ :: [a] -> [a] -> Queue a
mkQ [] –ys = Q (reverse ys) []
mkQ xs ys = Q xs ys





instance Cola Queue where
  vacia :: Queue a
  vacia = Q [] []

  esVacia :: Queue a -> Bool
  esVacia (Q xs _) = null xs

  poner :: a -> Queue a -> Queue a
  poner x (Q ys zs) = mkQ ys (x:zs)

  primero :: Queue a -> a   --parcial
  primero (Q (x:xs) _) = x

  sacar :: Queue a -> Queue a
  sacar (Q (x:xs) ys) = mkQ xs ys


rotar :: Cola q => Int -> q a -> q a
rotar 0 q = q
rotar n q = if esVacia q then q
                         else rotar (n-1)
                           (poner (primero q) (sacar q))


                           
