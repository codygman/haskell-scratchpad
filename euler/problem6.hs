-- Find the difference between the sum of the squares of the first one hundred natural numbers and the square of the sum.

problem6 = (sum $ map (\x -> x * x) [1..100]) - ((\x -> x * x) $ sum [1..100])

