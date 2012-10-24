-- Problem 31
-- Determine whether a given integer number is prime.

isPrime :: Int -> Bool
isPrime x | x < 2 = False
isPrime x = length (factors x) == 2

factors :: Int -> [Int]
factors x = filter (\y -> x `mod` y == 0) [1..x]

-- Problem 32
-- Determine the greatest common divisor of two positive integer numbers.  Use Euclid's algorithm.

myGCD :: Int -> Int -> Int
myGCD x y | x == y = x
myGCD x y = myGCD lesser (greater - lesser)
  where
    ax = abs x
    ay = abs y
    lesser = min ax ay
    greater = max ax ay

-- Problem 33
-- Determine whether two positive integer numbers are coprime. Two numbers are coprime if their greatest common divisor equals 1.

coprime :: Int -> Int -> Bool
coprime x y = gcd x y == 1

-- Problem 34
-- Calculate Euler's totient function phi(m).

totient :: Int -> Int
totient = length . filter id . coprimes
  where coprimes x = map (coprime x) [1..x]

-- Problem 35
-- Determine the prime factors of a given positive integer.  Construct a flat list containing the prime factors in ascending order.

primeFactors :: Int -> [Int]
primeFactors x | x == 1 = []
primeFactors x = a : primeFactors (x `div` a)
  where
    factors x = filter (\y -> x `mod` y == 0) [1..x]
    primeFacs x = filter isPrime $ factors x
    a = head $ primeFacs x

-- Problem 36
-- Determine the prime factors of a given positive integer.  Construct a list containing the prime factors and their multiplicity.

primeFactorsMult :: Int -> [(Int, Int)]
primeFactorsMult x = pfm as (a, 1)
  where
    (a:as) = primeFactors x
    pfm [] acc = [acc]
    pfm (x:xs) (a, c) | x == a = pfm xs (a, c+1)
    pfm (x:xs) (a, c) = (a, c) : (pfm xs (x, 1))

