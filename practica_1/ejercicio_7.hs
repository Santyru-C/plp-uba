genLista:: a -> (a -> a) -> Integer -> [a]
genLista ini f n = foldl (\acc _ -> acc ++ [f (last acc)]) [ini] [1..(n-1)]

desdeHasta:: Integer -> Integer -> [Integer]
desdeHasta x y = genLista x (\n -> n + 1) (y - x + 1)