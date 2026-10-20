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

--- Modelo Decodificacion 1 ---
codificacionTranspuesta :: String -> Int -> String
codificacionTranspuesta x n = codificar (cad2Matriz x n)

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

--- Modelo Decodicicacion 2 ---
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

--- Modelo Promedios ---
promedios :: [[Integer]] -> [Integer]
promedios x = map (promedio x)

promediosSinAplazos :: [[Integer]] -> [Integer]
pormediosSinAplazos x = map (pormedio . filter (>=4) ) x

promedio :: [Integer]  -> Float
promedio x  = (sum x) / (lengt x)

majorNota :: [[Integer]] -> [Integer]
mejorNota x = filter (<=10 x)

--- Modelo 1: Integración Numérica ---
integralPolinomi :: [Doubel] -> [Double]
integralPolinomi [] = []
integralPolinomio [x] = [x, 0]
integralPolinomi (x:xs) = (a / (length xs) + 1) : integralPolinomi xs

evaluar1 :: [Double] -> Double -> Double
evaluar1 [] _ = 0
evaluar1 (x:xs) n = (x * (n^(length xs)) ) + evaluar xs n

evaluar2 :: [Double] -> Double -> Double
evaluar2 [] _ = 0
evaluar2 (x:xs) n = ( (x / p) * (n ^ p) ) + evaluar2 xs n
    where p = length xs + 1

integralDefinida :: [Doubel] -> Doubel -> Doubel -> Double
integralDefinida [] _ _ = 0
integralDefinida polinomio la lb = evaluar1 (integrado lb) - evaluar1 (intergrado la)
    where integrado = integralPolinomi polinomio

--- Modelo 2: Geometría Analítica ---
distancia :: (Double, Double) -> (Double, Double) -> Double
distancia (x1, y1) (x2, y2) = sqrt ((x2 - x1)^2 + (y2 - y1)^2)

longitudRuta :: [(Double, Double)] -> Double
longitudRuta [] = 0
longitudRuta [_] = 0
longitudRuta (p1:p2:xss) = distancia p1 p2 + longitudRuta (p2:xss)

dentroDelRadio :: [(Double, Double)] -> (Double, Double) -> Double -> [(Double, Double)]
dentroDelRadio [] _ _  = []
dentroDelRadio (x:xs) punto r
    | (distancia x punto) <= r = x : dentroDelRadio xs punto r
    | otherwise = dentroDelRadio xs punto r

--- Modelo 3: Cifrado Dinámico ---
desplazarChar :: Char -> Int -> Char
desplazarChar c n
    | elem ( C L ) = L !! (mod p 26)
    | otherwise = c
    where C = toUpper c
    where L = ['A'..'C']
    where p = length ( takeWhile (/= C) L ) + n

extenderClave :: [Int] -> Int -> [Int]
extenderClave [] _ = []
extenderCleve x n = take n (cycle x)

cifrarDinamico :: String -> [Int] -> String
cifrarDinamico _ [] = []
cifrarDinamico [] _  = []
cifrafDinamico cdn clv = cifrafAux cdn (extenderClave clv (length cdn))

cifrarAux :: Sring -> [Int] -> Srting
cifrarAux _ [] = []
cifrarAux [] _ = []
cifrarAux (x:xs) (y:ys) = (desplazarChar x y) : cifrarAux xs ys

--- Modelo 4: Orden Superior y Datos Meteorológicos ---
temperaturasExtremas :: [[Float]] -> [Float]
mesesHelados :: [[Float]] -> [[Float]]
conteoDiasValidos :: [[Float]] -> [Int]

--- Modelo 5: Álgebra Lineal ---
productoEscalar :: [Double] -> [Double] -> Double
multiplicarPorEscalar :: Double -> [[Double]] -> [[Double]]
sumaVectores :: [Double] -> [Double] -> [Double]
sumaMatrices :: [[Double]] -> [[Double]] -> [[Double]]

--- Modelo 6: Regresión Lineal por Mínimos Cuadrados ---
sumatorias :: [(Double, Double)] -> (Double, Double, Double, Double)
calcularM :: [(Double, Double)] -> Double
ajusteLineal :: [(Double, Double)] -> (Double, Double)

--- Modelo 7: Aproximación por Series de Taylor ---
factorial :: Integer -> Double
terminoTaylor :: Double -> Integer -> Double
aproximarExponencial :: Double -> Integer -> Double

--- Modelo 8: Método de Bisección ---
biseccion :: (Double -> Double) -> Double -> Double -> Double -> Double

--- Modelo 9: Geometría de Polígonos ---
calcularTerminos :: [(Double, Double)] -> [Double]
areaPoligono :: [(Double, Double)] -> Double

--- Modelo 10: Integración Numérica ---
areaSubyacente :: (Double, Double) -> (Double, Double) -> Double
integralAproximada :: [(Double, Double)] -> Double

--- Modelo 11: Cifrado Afin ---
letraANumero :: Char -> Int y numeroALetra :: Int -> Char
cifrarLetraAfin :: Int -> Int -> Char -> Char
cifrarAfin :: Int -> Int -> String -> String

--- Modelo 12: Cifrado de Autoclave ---
generarClave :: String -> String -> String
cifrarAutoclave :: String -> String -> String

--- Modelo 13: Cifrado por Esteganografia deVectores ---
codificarAVector :: String -> [Double]
ocultarConRuido :: String -> [Double] -> [Double]
recuperarMensaje :: [Double] -> [Double] -> String

--- Modelo 14: Criptoanálisis - Análisis de Frecuencias ---
contarApariciones :: Char -> String -> Int
frecuencias :: String -> [Int]
esVocal :: Char -> Bool
extraerVocales :: String -> String