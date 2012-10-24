{- What is the largest prime factor of the number 600851475143 ? -}

prime x = x > 1 && factors x == [1,x]

factors x = facs ++ (reverse $ (dividends x facs))
  where
    facs           = [y | y <- [1..mid], x `mod` y == 0]
    mid            = floor . sqrt $ fromIntegral x
    dividends x ys = map (intdiv x) ys
    intdiv x y     = floor $ ((fromIntegral x) / (fromIntegral y))

problem3 = head (filter prime (reverse . factors $ 600851475143))

