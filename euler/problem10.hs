-- Find the sum of all the primes below two million.

prime x = x > 1 && factors x == [1,x]

factors x = facs ++ (reverse $ (dividends x facs))
  where
    facs           = [y | y <- [1..mid], x `mod` y == 0]
    mid            = floor . sqrt $ fromIntegral x
    dividends x ys = map (intdiv x) ys
    intdiv x y     = floor $ ((fromIntegral x) / (fromIntegral y))

problem10 = sum $ (filter prime (2:[3,5..2000000]))

