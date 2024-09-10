mapPares :: (a -> b -> c) -> [(a,b)] -> [c]
mapPares f = map (uncurry f)

mapPares2 :: (a -> b -> c) -> [(a, b)] -> [c]
mapPares2 f = foldr (\x rec -> uncurry f x : rec) [] 