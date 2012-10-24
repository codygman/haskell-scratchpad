{- By considering the terms in the Fibonacci sequence whose values do not exceed four million, find the sum of the even-valued terms. -}

fib a b = a : fib b (a + b)
fibs a  = (takeWhile (\b -> b < a) (fib 0 1))

problem2 = sum (filter even (fibs 4000000))

