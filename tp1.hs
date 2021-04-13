module TP1(
  Fabrica, Material (..),
  crearFabricaSimple,
  neg, esPar, sumarTuplas,
  foldMaterial, crearFabricaDeMaterial,
  secuenciar, paralelizar,
  pureza, filtrarPorPureza,
  emparejador, crearFabricaCompleja
  ) where

import Test.HUnit
import Data.List

-- Definiciones

-- Fábricas son funciones [a] -> [b]
type Fabrica a b = [a] -> [b]

-- Materiales son un tipo algebráico
data Material a =
  MateriaPrima a |
  Mezclar (Material a) Float (Material a)
    deriving (Eq, Show)

-- Ejercicio 1 a
-- Dada una función a->b crear una Fabrica a b
crearFabricaSimple = undefined

-- Ejercicio 1 b
-- Usando la función crearFabricaSimple, definir las siguientes fábricas:

-- neg que dada una lista de booleanos devuelve una lista con la negación de cada uno
neg :: Fabrica Bool Bool
neg = undefined

-- esPar que dada una lista de números enteros devuelve una lista que indica si cada uno es par
esPar :: Fabrica Int Bool
esPar = undefined

-- sumarTuplas que dada una lista de tuplas numéricas devuelve una lista con la suma de sus componentes
sumarTuplas :: Num a => Fabrica (a, a) a
sumarTuplas = undefined

-- Ejercicio 2
-- Definir el esquema de recursión estructural para el tipo Material
foldMaterial = undefined

-- Ejercicio 3
-- Dada una función a->b crear una fábrica que procese materiales de tipo a y produzca
-- materiales de tipo b aplicándole la función a cada caso base
crearFabricaDeMaterial :: (a -> b) -> Fabrica (Material a) (Material b)
crearFabricaDeMaterial = undefined

-- Ejercicio 4 a
-- Dadas dos fábricas simples, conectar la salida de la primera con la entrada de la segunda
secuenciar = undefined

-- Ejercicio 4 b
-- Cuando dos fábricas simples producen cosas del mismo tipo, estas se pueden paralelizar
-- De esta forma, dos fábricas simples se convierten en una sola que toma una lista de pares
-- (cada par contiene una entrada de cada una de las fábricas originales) y devuelve
-- una única lista que intercala los productos de ambas fábricas
paralelizar = undefined

-- Ejercicio 5 a
-- Dado un elemento y un material, determinar la pureza de dicho material
-- respecto a dicho elemento
pureza :: (Eq a) => a -> Material a -> Float
pureza = undefined

-- Ejercicio 5 b
-- Dada una lista de materiales y una lista de restricciones de pureza (representadas
-- como tuplas elemento-valor), filtrar los materiales en la primera lista
-- que cumplen con todas las restricciones de pureza en la segunda lista
filtrarPorPureza :: (Eq a) => [Material a] -> [(a,Float)] -> [Material a]
filtrarPorPureza = undefined

-- Ejercicio 6 a
-- Crear un emparejador
-- Un emparejador es una fábrica que en lugar de producir algo,
-- lo que hace es agrupar los materiales en pares
emparejador = undefined

paresEImpares :: [a] -> ([a], [a])
paresEImpares = foldr (\x rec -> (x:snd rec, fst rec)) ([], [])

-- Ejercicio 6 b
-- Dada una función a->a->b crear una Fabrica a b
-- Las fábricas complejas requieren dos unidades de material para producir cada unidad de producto
crearFabricaCompleja :: (a -> a -> b) -> Fabrica a b
crearFabricaCompleja = undefined

-- Tests
tests :: IO Counts
tests = do runTestTT allTests

allTests = test [
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6
  ]

-- Ejemplos sólo para mostrar cómo se escriben los tests. Reemplazar por los tests propios.

testsEj1 = test [
  2 ~=? 1+1,
  4 ~=? 2*2
  ]

testsEj2 = test [
  2 ~=? 1+1,
  4 ~=? 2*2
  ]

testsEj3 = test [
  2 ~=? 1+1,
  4 ~=? 2*2
  ]

testsEj4 = test [
  [True, False, True, True] ~=? secuenciar esPar neg [1, 2, 3, 5],
  [False, False, True, False, True, False] ~=? paralelizar neg esPar [(True, 1), (False, 3), (False, 1)]
  ]

verdad = MateriaPrima True
mentira = MateriaPrima False

testsEj5 = test [
  25.0 ~=? pureza True (Mezclar (Mezclar verdad 50.0 mentira) 50.0 mentira),
  [Mezclar verdad 80.0 mentira] ~=? filtrarPorPureza [Mezclar verdad 44.5 mentira, Mezclar verdad 80.0 mentira, Mezclar mentira 99.0 verdad] [(True, 50.0), (False , 1.0)]
  ]

testsEj6 = test [
  [(1, 2), (3, 4)] ~=? emparejador [1, 2, 3, 4],
  [5, 9, 11] ~=? crearFabricaCompleja (+) [2, 3, 12, -3, 11, 0]
  ]