-- many routes are there through a 20×20 grid?

problem15 :: Int -> Int -> Integer
problem15 _ 0 = 1
problem15 0 _ = 1
problem15 x y = problem15 (x-1) y + problem15 x (y-1)


problem15m :: Int -> Int -> Integer
problem15m w h = 1 + (sum $ take (h * w) p15')
  where p15' = (map p15'' [0..])
        p15'' :: Int -> Integer
        p15'' n
          | n < h          = 1
          | n `mod` h == 0 = 1
          | otherwise      = (p15' !! (n-1)) + (p15' !! (n-h))

