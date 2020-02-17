{-# LANGUAGE InstanceSigs #-}

module Heap where

import Functors

-- Una clase para montículos
class Heap h where
  empty :: h a

  singleton :: Ord a => a -> h a
  singleton a = insert a empty

  insert :: Ord a => a -> h a -> h a
  insert a h = (singleton a) `merge` h

  findMin :: Ord a => h a -> Maybe a

  deleteMin :: Ord a => h a -> h a

   -- heaps con merge
  merge :: Ord a => h a -> h a -> h a



  {-# MINIMAL empty, (singleton | insert), findMin, deleteMin, merge #-}


--Pairing Heap

data PHeap a = E | T a [PHeap a]
     deriving Show

-- Invariante: E nunca esta en la lista de un nodo T

instance Heap PHeap where
    empty :: PHeap a
    empty = E

    findMin :: PHeap a -> Maybe a
    findMin E = Nothing
    findMin (T x hs) = Just x

    singleton :: Ord a => a -> PHeap a
    singleton x = T x []

    merge :: Ord a => PHeap a -> PHeap a -> PHeap a
    merge E h = h
    merge h E = h
    merge h1@(T x hs1) h2@(T y hs2) | x < y     = T x (h2 : hs1)
                                    | otherwise = T y (h1 : hs2)

    deleteMin :: Ord a => PHeap a -> PHeap a
    deleteMin E        = E
    deleteMin (T x hs) = mergePairs hs
         where
          mergePairs :: Ord a => [PHeap a] -> PHeap a
          mergePairs [] = E
          mergePairs [h] = h
          mergePairs (h1 : h2 : hs) = merge (merge h1 h2) (mergePairs hs)


-- Implementando Heapsort

myheapsort :: Ord a => [a] -> [a]
myheapsort xs = listheap (fillheap xs)

fillheap :: Ord a => [a] -> PHeap a
fillheap [] = empty
fillheap (x:xs) = insert x (fillheap xs)
-- En fillheap, estamos eligiendo la implementación.

listheap :: (Heap h, Ord a) => h a -> [a]
listheap h = case findMin h of
         Nothing -> []
         Just x  -> x : listheap (deleteMin h)
