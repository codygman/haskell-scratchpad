-- What is the 10 001st prime number?

prime x = x > 1 && factors x == [1,x]

factors x = facs ++ (reverse $ (dividends x facs))
  where
    facs           = [y | y <- [1..mid], x `mod` y == 0]
    mid            = floor . sqrt $ fromIntegral x
    dividends x ys = map (intdiv x) ys
    intdiv x y     = floor $ ((fromIntegral x) / (fromIntegral y))

problem7 = head $ drop 9999 (filter prime [3,5..])

