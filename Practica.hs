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