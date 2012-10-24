-- Find the sum of the digits in the number 100!

fac x = product [1..x]

sumdigits :: Integral a => a -> a
sumdigits 0 = 0
sumdigits x = x `mod` 10 + sumdigits (x `div` 10)

problem20 = sumdigits $ fac 100
