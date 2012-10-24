-- Find the largest palindrome made from the product of two 3-digit numbers.

problem4 = maximum $ [ z | x <- [100..999], y <- [100..999], let z = x * y, (reverse . show $ (z)) == (show (z))]
