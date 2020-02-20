{-# LANGUAGE DeriveFunctor,
             GADTs,
             InstanceSigs,
             TypeOperators
 #-}

module Applicative where

import Functors hiding (map)
import Generics
import Control.Applicative

import Data.Monoid
import Data.Maybe

-- ------------------------
-- --FUNTORES APLICATIVOS
-- ------------------------

-- Generalizando fmap
-- ------------------

-- Veamos la clase funtor.

--  class Functor f where
--   fmap :: ( a ->   b)
--         -> f a -> f b

-- La función fmap nos da la forma de tomar una función pura de aridad 1
-- y "levantarla" al functor.







-- ¿Cómo sería una versión de fmap para
-- funciones de aridad arbitraria?

--  fmap0 ::   a
--        -> f a

--  fmap1 :: (  a ->   b)
--         -> f a -> f b

--  fmap2 :: (  a ->   b ->   c)
--         -> f a -> f b -> f c

--  fmap3 :: ( a ->   b ->   c ->   d)
--        -> f a -> f b -> f c -> f d












-- Creemos una clase con aridad 0 y aridad 2

class Functor f => MultiFunctor f where
   fmap0 :: a -> f a                           -- aridad 0
   fmap2 :: (a -> b -> c) -> f a -> f b -> f c -- aridad 2










-- Con esto nos alcanza para aridad mayores!

fmap3 :: MultiFunctor f => (  a ->   b ->   c ->   d)
                       ->  f a -> f b -> f c -> f d
fmap3 h fa fb fc = fmap2 ($) (fmap2 h fa fb) fc

{-
Analicemos los tipos:

  h       ::   a ->   b ->   (c -> d)
  fmap2 h :: f a -> f b -> f (c -> d)
  fmap2 h fa fb :: f (c -> d)

  fmap2 ($) :: f (c -> d) -> f c -> f d
  fmap2 ($) (fmap2 h fa fb) fc :: f d

El truco de la implementación está en notar que, mediante
currificación, una función de (1+n) argumentos es una función que
toma un argumento de tipo a y devuelve un tipo b (que toma n
argumentos) (a -> b),







Entonces podemos representar aridad 0 y aridad n+1.

 class Applicative f where
    pure   :: a -> f a                 -- aridad 0
    (<*>)  :: f (a -> b) -> f a -> f b --aridad (n+1)

 fmap0 f           = pure f
 fmap1 f fa        = pure f <*> f a
 fmap2 f fa fb     = pure f <*> fa <*> fb
 fmap3 f fa fb fc  = pure f <*> fa <*> fb <*> fc












----------------------------------------------------
-- Los funtores aplicativos nos permiten levantar --
--   funciones puras de cualquier aridad.         --
----------------------------------------------------







Notar que las clases de tipo Applicative y MultiFunctor son
equivalentes, ya que podemos implementar los métodos de una
clase con los métodos de la otra.

 fmap0 = pure
 fmap2 f fa fb = pure f <*> fa <*> fb

 (<*>) = fmap2 ($)

También como fmap1 tiene el mismo tipo que fmap, todo funtor
aplicativo puede tener una instancia de fmap. Por cómo funciona
el sistema de clases de tipo actualmente, para que el typechecker
lo sepa, es necesario agregar el requerimiento Functor a
la declaración de clase.

Por lo tanto, es equivalente, y más idiomático que como lo
escribimos más arriba, escribir estas definiciones usando el
operador infijo <$> (fmap):

 fmap1 f fa        = f <$> f a
 fmap2 f fa fb     = f <$> fa <*> fb
 fmap3 f fa fb fc  = f <$> fa <*> fb <*> fc







Definición de funtor aplicativo (McBride & Paterson 2008)
===============================

La clase Applicative está declarada entonces como:

 class Functor f => Applicative f where
   pure :: a -> f a
   (<*>) :: f (a -> b) -> f a -> f b

con las siguientes leyes:

identity:
    pure id <*> v = v

composition
    pure (.) <*> u <*> v <*> w = u <*> (v <*> w)

homomorphism
  pure f <*> pure x = pure (f x)

interchange
  u <*> pure y = pure ($ y) <*> u

-}




----------------------------------------------
-- EJEMPLOS
----------------------------------------------
{-
Si queremos computar con valores opcionales
podemos usar el aplicativo Maybe

data Maybe a = Nothing | Just a

instance Applicative Maybe where
    pure = Just
    Nothing <*> _       = Nothing
    _       <*> Nothing = Nothing
    Just f  <*> Just x  = Just (f x)

Por ejemplo:

> Just (+ 1) <*> Just 2
Just 3

> pure (+) <*> Just 1 <*> Just 2
Just 3

> (+) <$> Just 1 <*> Just 2
Just 3

> (+) <$> Nothing <*> Just 2
Nothing

Todas las computaciones aplicativas son de la forma
f <$> x <*> y <*> z

>(++) <$> Just "hola " <*> "mundo."
Just "hola mundo."
-}

dist :: Applicative f => [f a] -> f [a]
dist []     = pure []
dist (x:xs) = (:) <$> x <*> dist xs

mapFalluto :: (a -> Maybe b) -> [a] -> Maybe [b]
mapFalluto f = dist . map f



-----------------------------------------------------
{- Ejemplo: zipWith

la función zipWith junta dos listas aplicando una función de a pares

 zipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
 zipWith f [] bs = []
 zipWith f as [] = []
 zipWith f (a:as) (b:bs) = f a b : zipWith f as bs

El preludio provee también zipWith3

 zipWith3 :: (a -> b -> c -> d) -> [a] -> [b] -> [c] -> [d]

¿Qué pasa si queremos zipWith4?

Lo que podemos hacer es proveer a las listas con una instancia de
Applicative y así obtener zipWith de aridad arbitraria.

 newtype ZipList a = ZipList { getZipList :: [a] }
                   deriving (Functor)

 instance Applicative ZipList where
    pure :: a -> ZipList a
    pure a = ZipList (repeat a)

    (<*>) :: ZipList (a -> b) -> ZipList a -> ZipList b
    ZipList fs <*> ZipList xs = ZipList (zipWith ($) fs xs)

-}

zipWith3 :: (       a ->         b ->         c ->         d)
        -> ZipList a -> ZipList b -> ZipList c -> ZipList d
zipWith3 f as bs cs = pure f <*> as <*> bs <*> cs

zipWith4 :: (a -> b -> c -> d -> e)
         -> ZipList a -> ZipList b -> ZipList c -> ZipList d -> ZipList e
zipWith4 f as bs cs ds = pure f <*> as <*> bs <*> cs <*> ds




---------------------------------------------------
-- Modelando un entorno

data Exp a = Var a
           | Val Int
           | Add (Exp a) (Exp a)

type Env a = [(a,Int)]

evalS :: Eq a => Exp a -> Env a -> Int
evalS (Var e) env = fromMaybe 0 (lookup e env)
evalS (Val e) env = e
evalS (Add e1 e2) env = evalS e1 env + evalS e2 env

{-
 instance Applicative (e ->) where
    pure x = const x
    f <*> x = \e -> (f e) (x e)
-}

eval :: Eq a => Exp a -> Env a -> Int
eval (Var e) = fromMaybe 0 . lookup e
eval (Val e) = pure e
eval (Add e1 e2) = (+) <$> eval e1 <*> eval e2




-----------------------------------------------------
{-
Operadores Auxiliares
=====================

Algunos operadores auxiliares, para secuenciar computaciones
descartando la primera o la segunda computación.

-- Secuenciar y descartar el primer argumento
(*>) :: Applicative f => f a -> f b -> f b
a1 *> a2 = (const id <$> a1) <*> a2

-- Secuenciar y descartar el segundo argumento
(<*) :: Applicative f => f a -> f b -> f a
a1 <* a2 = const <$> a1 <*> a2

> Just 2 *> Just 3
Just 3

> Just 2 <* Just 3
Just 2

-}

---------------------------------------
-- El siguiente tipo de datos representa una computación
-- que dado el estado inicial, me devuelve un estado final,
-- junto con un valor.

data State s a = St { runSt :: s -> (s,a)}

instance Functor (State s) where
  fmap f (St g) = St (\s -> let (s',a) = g s
                            in (s', f a))

instance Applicative (State s) where
  pure x = St (\s -> (s,x))
  St sf <*> St sx = St $ \s -> let (s',f)  = sf s
                                   (s'',x) = sx s'
                               in (s'', f x)

cero :: State Int ()
cero = St (\s -> (0,()))

sucesor :: State Int ()
sucesor = St (\s -> (s+1,()))

ej1 :: State Int String
ej1 = cero *> sucesor *> pure "dos" <* sucesor

ej2 = sucesor *> ej1




---------------------------------------------------
-- Algebra de funtores aplicativos.
-- ================================

{- Ejercicio: Es el funtor Id es un aplicativo?
  Dar la instancia o argumentar por qué no lo es.
 -}

instance Applicative Id where
  pure = Id
  Id f <*> Id x = Id (f x)


{- Ejercicio: Es el producto de funtores aplicativos un aplicativo?
  Dar la instancia o argumentar por qué no lo es.
 -}

instance (Applicative f, Applicative g) => Applicative (f :*: g) where
  pure x = FunProd (pure x) (pure x)
  FunProd h h' <*> FunProd x x' = FunProd (h <*> x) (h' <*> x')

{- Ejercicio: Es el coproducto (suma) de funtores aplicativos
   un aplicativo? Dar la instancia o argumentar por qué no lo es.
 -}

 {- Ejercicio: Es la composición de funtores aplicativos un aplicativo?
   Dar la instancia o argumentar por qué no lo es.
  -}

instance (Applicative f, Applicative g) => Applicative (f :.: g) where
  pure x = FunComp (pure (pure x))
  FunComp fgh <*> FunComp fgx = FunComp $ (<*>) <$> fgh <*> fgx



-- ------------------------------------------------
{- Otro uso de los funtores aplicativos es representar acumuladores

newtype Const a b = Const { getConst :: a}

El tipo b es lo que se conoce como un "tipo fantasma". No juega ningún
papel en los datos.

El tipo Const a es un aplicativo, siempre que a sea un monoide.

instance Monoid a => Applicative (Const a) where
  pure x = Const mempty
  Const m1 <*> Const m2 = Const (m1 `mappend` m2)


 Usando algunos de los monoides de Bool (All o Any) podemos
 definir las siguientes funciones
-}

alguno :: (a -> Bool) -> [a] -> Bool
alguno p xs = getAny . getConst $ dist $ map (Const . Any . p) xs

todos :: (a -> Bool) -> [a] -> Bool
todos p xs = getAll . getConst $ dist $ map (Const . All . p) xs

----------------------------------------------
-- Funtor Aplicativo Libre
----------------------------------------------
{-
Dado un funtor f cualquiera podemos construir
el funtor aplicativo libre sobre f
(Capriotti & Kaposi 2014)
-}

data FreeA f a where
  Pure  :: a -> FreeA f a
  Apply :: f (b -> a) -> FreeA f b -> FreeA f a

instance Functor f => Functor (FreeA f) where
  fmap f x = pure f <*> x

instance Functor f => Applicative (FreeA f) where
  pure = Pure
  Pure f <*> x    = fmap f x
  Apply f x <*> y = Apply (fmap uncurry f) ((,) <$> x <*> y)

univA :: Applicative f => FreeA f a -> f a
univA (Pure a) = pure a
univA (Apply f x) = f <*> univA x
