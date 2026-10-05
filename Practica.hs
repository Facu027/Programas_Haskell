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