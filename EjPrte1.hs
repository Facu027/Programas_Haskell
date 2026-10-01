-- Ejercicios varios (Parte 1)

-- 1)
incTupla :: (Int, Int, Int) -> Int -> (Int, Int, Int)
incTupla (x, y, z) n = (x + n, y + n, z + n)
-- 2)
limpiar :: String -> String -> String
limpiar x [] = []
limpiar x (y:ys)
    | y `elem` x = limpiar x ys
    | otherwise = y : limpiar x ys
-- 3)
normalizar :: [Float] -> Float -> [Float]
normalizar  [] _ = []
normalizar (x:xs) p = (x - p) : normalizar xs p
promedio ::  [Float] -> Float
promedio [] = 0
promedio x = sum x / fromIntegral (length x)
-- 4)
-- no funcinoa con listas impares [1, 1, 3]
elemIguales :: [Integer] -> Bool
elemIguales [] = True
elemIguales [_] = True
elemIguales (x:y:xs)
    | x == y = elemIguales xs
    | otherwise = False
-- 5)
pares :: [a] -> [b] -> [(a,b)]
pares [] _ = []
pares _ [] = []
-- pares (x:xs) (y:ys) = pares xs ys ++ [(x,y)]
pares (x:xs) (y:ys) = (x,y) : pares xs ys
