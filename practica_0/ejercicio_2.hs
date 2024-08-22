valorAbsoluto :: Float -> Float
valorAbsoluto x = abs x

-- no pienso hacer OTRA VEZ la funcion biciesto, buscarla en la carpeta de algo1

factorial :: Int -> Int
factorial 0 = 1 -- nunca olvidar el caso base
factorial x = x * factorial (x - 1)

nFactores :: Int -> Int -> Int -> Int
nFactores n 0 i = i
nFactores n m i
    | mod n m == 0 = nFactores n (m - 1) (i + 1)
    | otherwise = nFactores n (m - 1) i

esPrimo :: Int -> Bool
esPrimo n
    | nFactores n n 0 == 2 = True
    | otherwise = False

auxCantDivisoresPrimos :: Int -> Int -> Int -> Int
auxCantDivisoresPrimos n 1 i = i
auxCantDivisoresPrimos n m i
    | mod n m == 0 && esPrimo m = auxCantDivisoresPrimos n (m - 1) (i + 1)
    | otherwise = auxCantDivisoresPrimos n (m - 1) i

cantDivisoresPrimos :: Int -> Int
cantDivisoresPrimos n = auxCantDivisoresPrimos n n 0


