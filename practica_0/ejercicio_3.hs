inverso :: Float -> Maybe Float
inverso 0 = Nothing
inverso x = Just (1/x)

aEntero :: Either Int Bool -> Int
aEntero (Left num) = num
aEntero (Right True) = 1
aEntero (Right False) = 0