import Data.List (transpose)

trsAList :: Int -> [Int]
trsAList 0 = []
trsAList n = trsAList (div n 10) ++ [mod n 10]

compara :: [Int] -> Int ->Bool
compara [] _ = True
compara (x:xs) n
    | x == (mod n 10) = compara xs (div n 10)
    | otherwise = False

capicua :: Int -> Bool
capicua n = compara (trsAList n) n

sumaParesMayores :: Int -> [Int] -> Int
sumaParesMayores _ [] = 0
sumaParesMayores n (x:xs)
    | (x > n && mod x 2 == 0) = x + sumaParesMayores n xs
    | otherwise = sumaParesMayores n xs

productosDisponibles :: [(String, Float, Int)] -> Float -> [String]
productosDisponibles [] _ = []
productosDisponibles ((nom, pre, stok):xs) n
    | (stok > 0 && pre <= n) = nom : productosDisponibles xs n
    | otherwise = productosDisponibles xs n

codicacionfTranspuesta :: String -> Int -> String
codicacionfTranspuesta x n = codificar (cad2Matriz x n)

codificar :: [String] -> String
codificar [] = []
codificar x = (primerElem2String x) ++ codificar (eliminoPrimerElem x)

primerElem2String :: [String] -> String
primerElem2String [] = []
primerElem2String ([]:_) = []
primerElem2String ((x:_):xss) = x : primerElem2String xss

eliminoPrimerElem :: [String] -> [String]
eliminoPrimerElem [] = []
eliminoPrimerElem ([]:_) = []
eliminoPrimerElem ((_:xs):xss) = xs : eliminoPrimerElem xss

cad2Matriz :: String -> Int -> [String]
cad2Matriz [] _ = []
cad2Matriz x n
    | n <= 0 = error "Numero no valido"
    | n <= length x = take n x : cad2Matriz (drop n x) n
    | otherwise = [completar x n]

completar :: String -> Int -> String
completar x n
    | (length x) < n = completar (x ++ ['*']) n
    | otherwise = x

decodificarTransposicion :: String -> Int -> String
decodificarTransposicion x n = sacoCaracter (codificar (cad2Matriz x fila))
    where fila = div (length x) n

sacoCaracter :: String -> String
sacoCaracter [] = []
sacoCaracter (x:xs)
    | x == '*' = sacoCaracter xs
    | otherwise = x : sacoCaracter xs

codificarTranspuesta2 :: String -> Int -> String
codificarTranspuesta2 [] _ = []
codificarTranspuesta2 x n = m2c (transpose (c2m (completar2 x n) n))

completar2 :: String -> Int -> String
completar2 x n = x ++ replicate tam '*'
    where tam  = ( n * ( (div (length x) n) + 1) ) - length x

c2m :: String -> Int -> [String]
c2m [] _ = []
c2m x n = (take n x) : c2m (drop n x) n

m2c :: [String] -> String
m2c [] = []
m2c (x:xs) = x ++ m2c xs

decodificarTransposicion2 :: String -> Int -> String
decodificarTransposicion2 x n = sacoCaracter (m2c (transpose (c2m x fila)))
    where fila = div (length x) n