-- 1)
bisiesto :: Int -> Bool
bisiesto x
    | mod x 4 == 0 = True
    | mod x 100 == 0 = False
    | otherwise = mod x 100 = True
-- 2)
contarPares :: [Int] -> Int
contarPares [] = 0
contarPares (x:xs)
    | mod x 10 == 0 = 1 + contarPares xs
    | otherwise = contarPares xs
-- 3)
producto :: [Int] -> Int
producto [] = 1
producto (x:xs) = x * producto xs
-- 4)
divisores :: Int -> [Int]
divisores x = [div | div <- [1..(x `div` 2)], mod x div == 0] ++ [x]
-- 5)
esPrimo :: Int -> Bool
esPrimo x
    | x < 2 = False
    | otherwise = length (divisores x) == 2
-- 6)
listaPrimos :: Int -> [Int]
listaPrimos n = [p | p <- [2..n], esPrimo p]
-- 7)
sumoCuadrados :: [Int] -> Int
sumoCuadrados [] = 0
sumoCuadrados (x:xs) = x^2 + sumoCuadrados xs
-- 8)
contarVocales :: String -> Int
contarVocales [] = 0
contarVocales (x:xs)
    | elem x "aeiouAEIOU" = 1 + contarVocales xs
    | otherwise = contarVocales xs
-- 9)
sumoDigitos :: [Int] -> Int
sumoDigitos [] = 0
sumoDigitos (x:xs)
    | x < 10 = x + sumoDigitos xs
    | otherwise = (digitos x) + sumoDigitos xs
-- 10)
digitos :: Int -> Int
digitos 0 = 0
digitos x = mod x 10 + digitos (div x 10)