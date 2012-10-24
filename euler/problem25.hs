--What is the first term in the Fibonacci sequence to contain 1000 digits?

import Data.List

fibs = map fib [0..]

fib :: Int -> Integer
fib 0 = 0
fib 1 = 1
fib n = fibs !! (n-1) + fibs !! (n-2)

problem25 = i
  where i = elemIndex x f
        x = head $ dropWhile (< 10^999) f
        f = fibs
