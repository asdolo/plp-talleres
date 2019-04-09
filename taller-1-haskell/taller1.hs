import Test.HUnit

-- Definiciones de tipos

data AB a = Nil | Bin (AB a) a (AB a) deriving Eq

instance Show a => Show (AB a) where
  show t = padAB t 0 0

-- Funciones auxiliares

pad :: Int -> String
pad i = replicate i ' '

padAB :: Show a => AB a -> Int -> Int -> String
padAB = foldAB (const $ const "") (\ri x rd n base ->let l = length $ show x in pad n ++ show x ++ ri 4 (base+l) ++ "\n" ++ rd (n+4+base+l) base)

-- Crea una hoja de un árbol binario AB
abHoja :: a -> AB a
abHoja x = Bin Nil x Nil

-- Devuelve una lista con los elementos de los nodos de un árbol binario AB recorridos en profundidad de izquierda a derecha
inorder :: AB a -> [a]    
inorder = foldAB [] (\i r d -> i ++ (r:d))

-- Estructuras para tests

-- Heap (<) completo
ab1 = Bin (abHoja 4) 2 (abHoja 5)
-- Heap (<) completo
ab2 = Bin (abHoja 6) 3 (abHoja 7)
-- Heap (>) completo
ab3 = Bin (Bin (abHoja 4) 5 (abHoja 2)) 7 (Bin (abHoja 3) 6 (abHoja 1))
-- Heap (<)
ab4 = Bin ab1 1 (abHoja 3)
-- ABB completo
ab5 = Bin (Bin (abHoja 1) 2 (abHoja 3)) 4 (Bin (abHoja 5) 6 (abHoja 7))
-- Heap (<)
ab6 = Bin ab1 0 (abHoja 6)
-- ABB
ab7 = Bin (Bin (abHoja 1) 2 (abHoja 4)) 5 (abHoja 7)
-- Heap (<) infinito, probar truncando
ab8 = Bin (mapAB (*2) ab8) 1 (mapAB ((+1) . (*2)) ab8)

-- Ejercicios

--recAB :: 
recAB = undefined

--foldAB 
foldAB = undefined

mapAB :: (a -> b) -> AB a -> AB b
mapAB = undefined

nilOCumple :: (a -> a -> Bool) -> a -> AB a -> Bool
nilOCumple = undefined

esABB :: Ord a => AB a -> Bool
esABB = undefined

esHeap :: (a -> a -> Bool)  -> AB a -> Bool
esHeap = undefined

completo :: AB a -> Bool
completo = undefined

insertarABB :: Ord a => AB a -> a -> AB a
insertarABB = undefined

insertarHeap :: (a -> a -> Bool) -> AB a -> a -> AB a
insertarHeap undefined 

truncar :: AB a -> Integer -> AB a
truncar = undefined

--Ejecución de los tests
main :: IO Counts
main = do runTestTT allTests

allTests = test [
  "ejercicio1" ~: testsEj1,
  "ejercicio2" ~: testsEj2,
  "ejercicio3" ~: testsEj3,
  "ejercicio4" ~: testsEj4,
  "ejercicio5" ~: testsEj5,
  "ejercicio6" ~: testsEj6,
  "ejercicio7" ~: testsEj7
  ]

testsEj1 = test [
  [1,2,4,5,7] ~=? inorder ab7,
  [1,2,3,4,5,6,7] ~=? inorder ab5
  ]
  
testsEj2 = test [
  [5,3,6,1,7] ~=? inorder (mapAB (+1) ab6)
  ]

testsEj3 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj4 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj5 = test [
  0 ~=? 0 --Cambiar esto por tests verdaderos.
  ]

testsEj6 = test [
  True ~=? esHeap (<) (insertarHeap (<) (insertarHeap (<) ab6 3) 1),
  True ~=? esABB (insertarABB (insertarABB ab7 6) 9)
  ]

testsEj7 = test [
  [8,4,12,2,10,6,14,1,9,5,13,3,11,7,15] ~=? inorder (truncar ab8 4),
  True ~=? esHeap (<) (truncar ab8 5)
  ]
