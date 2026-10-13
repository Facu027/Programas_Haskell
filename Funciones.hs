--- Operaciiones para Listas ---
head :: [a] -> a             --- devuelve el primer elemento de la lista ---
last :: [a] -> a             --- devuelve el ultimo elemento de la lista ---
tail :: [a] -> [a]           --- devuelve la lista menos el primero elemento ---
init :: [a] -> [a]           --- devuelve la lista menos el ultimo elemento ---
langth :: [a] -> Int         --- devuelve el tamaño de la lista ---
null :: [a] -> Bool          --- verifica si una lista esta vacia ---
take :: Int -> [a] -> [a]    --- toma los primeros n elementos ---
drop :: Int -> [a] -> [a]    --- elimina los primeros n elementos ---
reverse :: [a] -> [a]        --- invierte el orden la los elemento de la lista ---
replicate :: Int -> a -> [a] --- crea una lista de n elementos ---
repeat :: a -> [a]           --- crea una lista infinita de el mismo elemento---

--- Funciones de Orden superios ---
map :: (a -> b) -> [a] -> [b]           --- aplica una funcion a cada elemento de la lista y devuelde la lista modificada ---
fliter :: (a -> Bool) -> [a] -> [b]     --- aplica una condicion a cada elemento de la lista y devuelde la lista modificada ---
foldl :: (b -> a -> b) -> b -> [a] -> b --- reduce una lista a un solo valor aplicando una función de izquierda a derecha, partiendo de un valor inicial (acumulador) ---
foldr :: (a -> b -> b) -> b -> [a] -> b --- igual que foldl, pero opera de derecha a izquierda ---

--- Operaciones Logicas ---
even :: Integrala a => a -> Bool --- devuelve verdadero si el numero es par ---
odd :: Integral a => a -> Bool   --- devuelve verdadero si el numero es inpar ---

--- Operacines para Tuplas ---
fst :: (a, b) -> a            --- devuelve el primer componente de la tupla ---
snd :: (a, b) -> b            --- devuelve el segundo componente de la tupla ---
zip :: [a] -> [b] -> [(a, b)] --- toma dos lista y las empareja formando una lista de tuplas ---

--- Operacines para Matrizes ---
transpose :: [[a]] -> [[a]]           --- convierte las filas de una matriz en colubnas y viceversa ---
concat :: [[a]] -> [a]                --- convierte una matriz en una lista simple (aplanar) ---
concatMap :: (a -> [b]) -> [a] -> [b] --- aplica primero un map y luego un concat ---
