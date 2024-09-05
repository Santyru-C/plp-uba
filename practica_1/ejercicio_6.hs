recr :: (a -> [a] -> b -> b) -> b -> [a] -> b
recr _ z [] = z
recr f z (x:xs) = f x xs (recr f z xs)

sacarUna :: Eq a => a -> [a] -> [a]
sacarUna e = recr (\h t rec -> if h == e then t else h : rec) []

insertarOrdenado :: Ord a => a -> [a] -> [a] 
insertarOrdenado e = recr (\x xs rec -> if null xs then x:e:rec else if e < head xs then x:e:xs else x : rec) [] 