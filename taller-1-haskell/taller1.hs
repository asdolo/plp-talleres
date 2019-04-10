--import Test.HUnit

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
-- Caso borde de ABB 1
ab9 = Bin (Bin (abHoja 1) 2 (abHoja 4)) 3 (abHoja 5)
-- Caso borde de ABB 2
ab10 = Bin (abHoja 2) 3 (Bin (abHoja 1) 5 (abHoja 10))


-- Ejercicios

recAB :: b -> (AB a -> a -> AB a -> b -> b -> b) -> AB a -> b
recAB z _ Nil = z
recAB z f (Bin izq x der) = f izq x der recizq recder
  where recizq = recAB z f izq
        recder = recAB z f der

foldAB :: b -> (b -> a -> b -> b) -> AB a -> b
foldAB z f = recAB z fRec
  where fRec = \_ n _ rIzq rDer -> f rIzq n rDer

mapAB :: (a -> b) -> AB a -> AB b
mapAB = undefined

nilOCumple :: (a -> a -> Bool) -> a -> AB a -> Bool
nilOCumple c x ab = foldAB True (\_ n _ -> x `c` n) ab

esABB :: Ord a => AB a -> Bool
esABB = recAB True fRec
  where fRec            = \izq n der rIzq rDer -> (g izq n der) && rIzq && rDer
        g               = \izq n der -> (esMayorQueTodos n izq) && (esMenorQueTodos n der)
        esMayorQueTodos = \x ab -> x >= (foldAB x (\rI n rD -> maximum [rI,n,rD]) ab)
        esMenorQueTodos = \x ab -> x <= (foldAB x (\rI n rD -> minimum [rI,n,rD]) ab)

esHeap :: (a -> a -> Bool)  -> AB a -> Bool
esHeap c = recAB True f
  where f = \izq n der rIzq rDer -> (g izq n der) && rIzq && rDer
        g = \izq n der -> (nilOCumple c n izq) && (nilOCumple c n der)

completo :: AB a -> Bool
completo ab = (2^(altura ab) - 1) == (cantNodos ab)
  where altura = foldAB 0 (\rIzq n rDer -> 1 + (max rIzq rDer))
        cantNodos = foldAB 0 (\rIzq n rDer -> 1 + rIzq + rDer)

insertarABB :: Ord a => AB a -> a -> AB a
insertarABB ab x = recAB (abHoja x) (\izq n der rIzq rDer -> if x > n 
                                                             then (Bin izq n rDer)
                                                             else (Bin rIzq n der)) ab

swapHeap :: (a -> a -> Bool) -> AB a -> a -> (a, AB a)
swapHeap c (Bin i e d) x = if e `c` x then (e, Bin i x d) else (x, Bin i e d)

swapDer = (\i e d c -> Bin i (fst (swapHeap c d e)) (snd (swapHeap c d e)))

swapIzq = (\i e d c -> Bin (snd (swapHeap c i e)) (fst (swapHeap c i e)) d)


insertarHeap :: (a -> a -> Bool) -> AB a -> a -> AB a
insertarHeap c ab x = recAB (abHoja x) (\izq n der rIzq rDer -> if ((completo izq) && (cantNodos izq) > (cantNodos der)) 
                                                                then (swapDer izq n rDer c) else (swapIzq rIzq n der c)) ab
  where cantNodos = foldAB 0 (\rIzq n rDer -> 1 + rIzq + rDer)



truncar :: AB a -> Integer -> AB a
truncar = undefined

--Ejecución de los tests
--main :: IO Counts
--main = do runTestTT allTests
--
--allTests = test [
--  "ejercicio1" ~: testsEj1,
--  "ejercicio2" ~: testsEj2,
--  "ejercicio3" ~: testsEj3,
--  "ejercicio4" ~: testsEj4,
--  "ejercicio5" ~: testsEj5,
--  "ejercicio6" ~: testsEj6,
--  "ejercicio7" ~: testsEj7
--  ]
--
--testsEj1 = test [
--  [1,2,4,5,7] ~=? inorder ab7,
--  [1,2,3,4,5,6,7] ~=? inorder ab5
--  ]

--  
--testsEj2 = test [
--  [5,3,6,1,7] ~=? inorder (mapAB (+1) ab6)
--  ]
--
--testsEj3 = test [
--  0 ~=? 0 --Cambiar esto por tests verdaderos.
--  ]
--
--testsEj4 = test [
--  0 ~=? 0 --Cambiar esto por tests verdaderos.
--  ]
--
--testsEj5 = test [
--  0 ~=? 0 --Cambiar esto por tests verdaderos.
--  ]
--
--testsEj6 = test [
--  True ~=? esHeap (<) (insertarHeap (<) (insertarHeap (<) ab6 3) 1),
--  True ~=? esABB (insertarABB (insertarABB ab7 6) 9)
--  ]
--
--testsEj7 = test [
--  [8,4,12,2,10,6,14,1,9,5,13,3,11,7,15] ~=? inorder (truncar ab8 4),
--  True ~=? esHeap (<) (truncar ab8 5)
--  ]
--