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
crearFabricaSimple :: (a -> b) -> Fabrica a b
crearFabricaSimple f = (\xs -> map f xs)

-- Ejercicio 1 b
-- Usando la función crearFabricaSimple, definir las siguientes fábricas:

-- neg que dada una lista de booleanos devuelve una lista con la negación de cada uno
neg :: Fabrica Bool Bool
neg = crearFabricaSimple not

-- esPar que dada una lista de números enteros devuelve una lista que indica si cada uno es par
esPar :: Fabrica Int Bool
esPar = crearFabricaSimple even

-- sumarTuplas que dada una lista de tuplas numéricas devuelve una lista con la suma de sus componentes
sumarTuplas :: Num a => Fabrica (a, a) a
sumarTuplas = crearFabricaSimple (\p -> (fst p) + (snd p))

-- Ejercicio 2
-- Definir el esquema de recursión estructural para el tipo Material
foldMaterial :: (a -> b) -> (b -> Float -> b -> b) -> Material a -> b
foldMaterial fMateriaPrima fMezclar m = case m of 
    (MateriaPrima e) -> fMateriaPrima e
    (Mezclar m1 p m2) -> fMezclar (rec m1) p (rec m2)
        where rec = foldMaterial fMateriaPrima fMezclar

-- Ejercicio 3
-- Dada una función a->b crear una fábrica que procese materiales de tipo a y produzca
-- materiales de tipo b aplicándole la función a cada caso base
crearFabricaDeMaterial :: (a -> b) -> Fabrica (Material a) (Material b)
crearFabricaDeMaterial f = crearFabricaSimple (foldMaterial (\mp -> MateriaPrima (f mp)) (\rec1 p rec2 -> Mezclar rec1 p rec2))

-- Ejercicio 4 a
-- Dadas dos fábricas simples, conectar la salida de la primera con la entrada de la segunda
secuenciar :: Fabrica a b -> Fabrica b c -> Fabrica a c 
secuenciar f1 f2 = (\xs -> f2 $ f1 xs)

-- Ejercicio 4 b
-- Cuando dos fábricas simples producen cosas del mismo tipo, estas se pueden paralelizar
-- De esta forma, dos fábricas simples se convierten en una sola que toma una lista de pares
-- (cada par contiene una entrada de cada una de las fábricas originales) y devuelve
-- una única lista que intercala los productos de ambas fábricas
paralelizar :: Fabrica a c -> Fabrica b c -> Fabrica (a, b) c 
paralelizar f1 f2 = (\xs -> entrelazar $ zip (f1 (map fst xs)) (f2 (map snd xs)))

entrelazar :: [(a, a)] -> [a]
entrelazar = concatMap (\(x,y) -> [x,y])

-- Ejercicio 5 a
-- Dado un elemento y un material, determinar la pureza de dicho material
-- respecto a dicho elemento
pureza :: (Eq a) => a -> Material a -> Float
pureza e m = foldMaterial (\mp -> if mp == e then 100 else 0) (\rec1 p rec2 -> rec1 * (p/100) + rec2 * (1 - (p/100))) m

-- Ejercicio 5 b
-- Dada una lista de materiales y una lista de restricciones de pureza (representadas
-- como tuplas elemento-valor), filtrar los materiales en la primera lista
-- que cumplen con todas las restricciones de pureza en la segunda lista
filtrarPorPureza :: (Eq a) => [Material a] -> [(a,Float)] -> [Material a]
filtrarPorPureza ms rs = filter ((flip cumpleRestricciones) rs) ms  

cumpleRestricciones :: (Eq a) => Material a -> [(a, Float)] -> Bool
cumpleRestricciones m rs = foldr (&&) True (map (\r -> (pureza (fst r) m) >= (snd r)) rs)

-- Ejercicio 6 a
-- Crear un emparejador
-- Un emparejador es una fábrica que en lugar de producir algo,
-- lo que hace es agrupar los materiales en pares
emparejador :: Fabrica a (a,a)
emparejador = uncurry zip . paresEImpares

paresEImpares :: [a] -> ([a], [a])
paresEImpares = foldr (\x rec -> (x:snd rec, fst rec)) ([], [])

-- Ejercicio 6 b
-- Dada una función a->a->b crear una Fabrica a b
-- Las fábricas complejas requieren dos unidades de material para producir cada unidad de producto
crearFabricaCompleja :: (a -> a -> b) -> Fabrica a b
crearFabricaCompleja f = map (uncurry f) . emparejador

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
  [] ~=? crearFabricaSimple (+1) [],
  [1, 2] ~=? crearFabricaSimple (+1) [0, 1],
  [2, 4] ~=? crearFabricaSimple (*2) [1, 2],
  [False, True] ~=? neg [True, False],
  [False, True] ~=? esPar [1, 2],
  [3, 8] ~=? sumarTuplas [(1,2), (5,3)] 
  ]

auxTestsEj2 = foldMaterial not (\m1 p m2 -> if p > 50 then m1 else m2)
testsEj2 = test [
  False ~=? auxTestsEj2 (MateriaPrima True),
  False ~=? auxTestsEj2 (Mezclar (MateriaPrima True) 51 (MateriaPrima False)) 
  ]

testsEj3 = test [
  [MateriaPrima 4] ~=? crearFabricaDeMaterial (+1) [MateriaPrima 3],
  [(Mezclar (MateriaPrima 6) 50 (MateriaPrima 8)), MateriaPrima 4] ~=? crearFabricaDeMaterial (+1) [(Mezclar (MateriaPrima 5) 50 (MateriaPrima 7)), MateriaPrima 3]
  ]

testsEj4 = test [
  [True, False, True, True] ~=? secuenciar esPar neg [1, 2, 3, 5],
  [False, False, True, False, True, False] ~=? paralelizar neg esPar [(True, 1), (False, 3), (False, 1)]
  ]

verdad = MateriaPrima True
mentira = MateriaPrima False

testsEj5 = test [
  25.0 ~=? pureza True (Mezclar (Mezclar verdad 50.0 mentira) 50.0 mentira),
  100.0 ~=? pureza True (Mezclar (Mezclar mentira 0.0 verdad) 100.0 verdad),
  0.0 ~=? pureza True (Mezclar (Mezclar mentira 100.0 verdad) 100.0 verdad),
  [Mezclar verdad 80.0 mentira] ~=? filtrarPorPureza [Mezclar verdad 44.5 mentira, Mezclar verdad 80.0 mentira, Mezclar mentira 99.0 verdad] [(True, 50.0), (False , 1.0)]
  ]

testsEj6 = test [
  [(1, 2), (3, 4)] ~=? emparejador [1, 2, 3, 4],
  [5, 9, 11] ~=? crearFabricaCompleja (+) [2, 3, 12, -3, 11, 0]
  ]