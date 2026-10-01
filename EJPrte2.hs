import Data.Char (toUpper)
-- Ejercicios varios (Parte 2)

-- 1)
pertenece :: Eq a => a -> [a] -> Bool
pertenece _ [] = False
pertenece x (y:ys)
    | x == y = True
    | otherwise = pertenece x ys
-- 2)
interseccion :: Eq a => [a] -> [a] -> [a]
interseccion [] _ = []
interseccion (x:xs) ys
    | pertenece x ys = x : interseccion xs ys
    | otherwise = interseccion xs ys
-- 3)
sinRepetidos :: Eq a => [a] -> [a]
sinRepetidos [] = []
sinRepetidos (x:xs)
    | pertenece x xs = sinRepetidos xs
    | otherwise = x : sinRepetidos xs
-- 4)
union :: Eq a => [a] -> [a] -> [a]
union [] ys = ys
union (x:xs) ys
    | pertenece x ys = union xs ys
    | otherwise = x : union xs ys
-- 5)
digitoEntero :: [Int] -> Int
digitoEntero [] = 10
digitoEntero (x:xs) = (x * (10 ^ length xs)) + digitoEntero xs
-- 6)
codigoMurcielago :: String -> String
codigoMurcielago x = map sustituye x
sustituye :: Char -> Char
sustituye c = case toUpper c of
    'M' -> '0'
    'U' -> '1'
    'R' -> '2'
    'C' -> '3'
    'I' -> '4'
    'E' -> '5'
    'L' -> '6'
    'A' -> '7'
    'G' -> '8'
    'O' -> '9'
    '0' -> 'M'
    '1' -> 'U'
    '2' -> 'R'
    '3' -> 'C'
    '4' -> 'I'
    '5' -> 'E'
    '6' -> 'L'
    '7' -> 'A'
    '8' -> 'G'
    '9' -> 'O'
    _   -> c