{- http://chaoxuprime.com/files/works/other/haskell-hw/hw1.pdf -}

{- 1. Produce an infinite list, such that the kth element in the list (indexed from 0) is the k + 1th prime. -}

factors a = filter (\x -> a `mod` x == 0) [2..floor . sqrt $ fromIntegral a]

isPrime a = (length (factors a)) == 0 && a /= 1

problem1 = filter isPrime [2..]
