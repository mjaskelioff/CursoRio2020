{-# LANGUAGE DeriveFunctor,
             ExistentialQuantification,
             RankNTypes,
             InstanceSigs,
             TypeOperators
 #-}

module Alternative where

import Applicative
import Control.Applicative
import Data.Char

----------------------------------------------
-- Alternative Functors
----------------------------------------------
{-
class Applicative f => Alternative f where
  empty :: f a
  (<|>) :: f a -> f a -> f a








No hay consenso sobre qué leyes deben satisfacer sus instancias.
Sí hay consenso que estas operaciones deben ser un monoide.

empty <|> x = x
x <|> empty = x
(x <|> y) <|> z = x <|> (y <|> z)











Pero ¿cómo deben interactuar con las operaciones de Applicative?
Depende de lo que uno quiera modelar. Para modelar no-determinismo,
las ecuaciones son (Rivas, Jaskelioff, Schrijvers (2018)):

 empty <*> x = empty
 (f <|> g) <*> x = (f <*> x) <|> (g <*> x)









 Ejemplo de instancia

instance Alternative Maybe where
  empty = Nothing
  Nothing <|> r = r
  l       <|> _ = l

-}






-- -- Aplicación: Parsers
class Alternative f => AppParser f where
  sat :: (Char -> Bool) -> f Char
  eof :: f ()










  {-
  La clase Alternative ya trae definido las siguientes funciones
  (que funcionan para cualquier aplicativo,
  no solamente para Parsers)

  -- una o mas veces
  some :: Alternative f => f a -> f [a]
  some v = some_v
      where
        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v

  -- cero o mas veces
  many :: Alternative f => f a -> f [a]
  many v = many_v
      where
        many_v = some_v <|> pure []
        some_v = (:) <$> v <*> many_v
  -}

  ---- operaciones derivadas
item            :: AppParser parser => parser Char
item             = sat (const True)

char            :: AppParser parser => Char -> parser Char
char c           = sat (c ==)

string          :: AppParser parser => String -> parser String
string ""        = pure ""
string (c:cs)    = (:) <$> char c <*> string cs

sepby           :: AppParser parser => parser a -> parser b -> parser [a]
p `sepby` sep    = (p `sepby1` sep) <|> pure []

sepby1          :: AppParser parser => parser a -> parser b -> parser [a]
p `sepby1` sep   = (:) <$> p <*> many (sep *> p)

nat :: AppParser parser => parser Int
nat = read <$> some (sat isDigit)

-- Ignorando espacios
ws :: AppParser parser => parser ()
ws = many (sat isSpace) *> pure ()

token :: AppParser parser => parser a -> parser a
token p = ws *> p <* ws

symbol   :: AppParser parser => String -> parser String
symbol xs = token (string xs)

-------------------------------
-- Una instancia de AppParser
--------------------------------
newtype Parser a = P {parse :: String -> Maybe (a,String)}
              deriving Functor

instance Applicative Parser where
   pure x = P $ \s -> pure (x,s)
   P f <*> P x = P $ \s ->
                    case f s of
                      Nothing      -> Nothing
                      Just (g,s')  -> case x s' of
                        Nothing      -> Nothing
                        Just (y,s'') -> Just (g y,s'')

instance Alternative Parser where
  empty = P (const Nothing)
  P a <|> P b = P $ \s -> a s <|> b s









 -- newtype Parser a = P {parse :: String -> Maybe (a,String)}

instance AppParser Parser where
  sat :: (Char -> Bool) -> Parser Char
  sat p = P  $ \s -> case s of
                    (c:cs) | p c       -> Just (c,cs)
                           | otherwise -> Nothing
                    ""                 -> Nothing
  eof :: Parser ()
  eof = P $ \s -> if null s then Just ((),"") else Nothing






-----------------
-- Ejemplos
-----------------

listas :: AppParser p => p [Int]
listas =  token (char '[')
      *>  nat `sepby` token (char ',')
      <* token (char ']')



nl = char ';'

p1 :: Parser Int
p1 =  symbol  "begin" *> nl
  *>  symbol "return" *>  token nat
  <* token nl

s1 = "begin; return 234 ;   begin;  return 12;"

test = parse p1

{- Ejercicio: Hacer un parser para identificadores, donde un identificador es
una cadena que empieza con una minúscula y sigue con
cualquier caracter alfanumérico
-}


{- Ejercicio
  Chequear que una cadena de paréntesis está bien balanceada con un parser

 parentesis :: Parser ()

  Por ejemplo
  > parse parentesis  "(()())()"
  Just ((),"")

  > parse parentesis  "(()()()"
  Nothing
-}

{- Ejercicio: Evaluador de expresiones aritméticas

  Dada la gramática BNF de expresiones aritméticas

  expr ::= term (’+’ expr | ε )
  term ::= factor (’*’ term | ε )
  factor ::= nat | ’(’ expr ’)’

  (ε denota la cadena vacía y nat es un número natural)

  Dar una función

  evalExpr :: String -> Maybe Int

  que parsee expresiones y las evalúe.

  Por ejemplo:

  > evalExpr "2*3+4*(1+4)"
Just 26

  Recomendamos definir un parser para cada no terminal.

-}
{- Ejercicio**

Extender el ejercicio anterior para que maneje resta y división.
Por ejemplo

> evalExpr "10-2-1"
Just 7

-}
