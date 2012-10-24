pack :: Eq a => [a] -> [[a]]
pack = foldr pack' []
    where pack' x [] = [[x]]
          pack' x (y:xs) = if x == (head y)
            then ((x:y):xs)
            else ([x]:y:xs)

encode :: Eq a => [a] -> [(Int,a)]
encode = fmap (\x -> (length x, head x)) . pack

data RunLength a = Single a | Multiple Int a deriving Show

encodeModified :: Eq a => [a] -> [RunLength a]
encodeModified = (map runLength) . encode
  where runLength x
          | fst x == 1 = Single $ snd x
          | otherwise  = Multiple (fst x) (snd x)

decodeModified :: Eq a => [RunLength a] -> [a]
decodeModified [] = []
decodeModified (x:xs) = decodeModified' x ++ decodeModified xs
  where decodeModified' (Single a)     = [a]
        decodeModified' (Multiple n a) = replicate n a

encodeDirect :: Eq a => [a] -> [RunLength a]
encodeDirect [] = []
encodeDirect (x:xs) = encodeDirect' front : encodeDirect rear
  where front = takeWhile (== x) (x:xs)
        rear  = dropWhile (== x) (x:xs)
        encodeDirect' (y:[]) = Single y
        encodeDirect' (y:ys) = Multiple (length (y:ys)) y

dupli :: [a] -> [a]
dupli [] = []
dupli (x:xs) = x:x:dupli xs

repli :: [a] -> Int -> [a]
repli [] _ = []
repli (x:xs) n = (replicate n x) ++ repli xs n

dropEvery :: [a] -> Int -> [a]
dropEvery as x = dropEvery' as x where
  dropEvery' [] _ = []
  dropEvery' (a:as') 1 = dropEvery as' x
  dropEvery' (a:as') x = a : dropEvery' as' (x-1) 

split :: [a] -> Int -> ([a], [a])
split as x = (take' x as, drop' x as) where
  take' 0 as'     = []
  take' _ []      = []
  take' x (a:as') = a : take' (x-1) as'
  drop' 0 as'     = as'
  drop' _ []      = []
  drop' x (a:as') = drop' (x-1) as'

slice :: [a] -> Int -> Int -> [a]
slice as x y = take' (y-(x-1)) (drop' (x-1) as) where
  take' 0 as'     = []
  take' _ []      = []
  take' x (a:as') = a : take' (x-1) as'
  drop' 0 as'     = as'
  drop' _ []      = []
  drop' x (a:as') = drop' (x-1) as'

rotate :: [a] -> Int -> [a]
rotate as 0 = as
rotate [] _ = []
rotate as@(a:as') x
  | x < 0     = rotate as (length as + x)
  | otherwise = rotate (as' ++ [a]) (x-1)

removeAt :: Int -> [a] -> (a, [a])
removeAt x as = (head $ drop' (x) as, take' x as ++ drop' (x+1) as) where
  take' 0 as'     = []
  take' _ []      = []
  take' x (a:as') = a : take' (x-1) as'
  drop' 0 as'     = as'
  drop' _ []      = []
  drop' x (a:as') = drop' (x-1) as'

