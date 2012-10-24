import System.Random (randomRIO)
import System.IO.Unsafe (unsafePerformIO)
import Data.List (delete)

insertAt :: Char -> String -> Int -> String
insertAt c s i = (take (i-1) s) ++ [c] ++ (drop (i-1) s)

range :: Int -> Int -> [Int]
range x y
  | x == y = [x]
  | otherwise = x : range (x+1) y

rnd_select :: [a] -> Int -> IO [a]
rnd_select _  0 = return []
rnd_select xs i = do
  index <- randomRIO (0, length xs - 1)
  let head = xs !! index 
  tail <- rnd_select xs (i-1)
  return (head:tail)

rnd_select' :: [a] -> Int -> [a]
rnd_select' _  0 = []
rnd_select' xs i = (head:tail) where
  index = unsafePerformIO $ randomRIO (0, length xs - 1)
  head = xs !! index 
  tail = rnd_select' xs (i-1)

diff_select :: Int -> Int -> IO [Int]
diff_select count max = take_rnd count [0..max] where
  take_rnd 0      _    = return []
  take_rnd count2 list = do
    index <- randomRIO (0, length list - 1)
    let rnd = list !! index
    rest <- take_rnd (count2-1) ((take index list) ++ (drop (index+1) list))
    return (rnd:rest)

rnd_permu :: [a] -> IO [a]
rnd_permu [] = return []
rnd_permu xs = do 
  idx <- randomRIO (0, length xs - 1)
  tail <- rnd_permu (take idx xs ++ drop (idx + 1) xs)
  return ((xs !! idx) : tail)

ordered_combinations :: Eq a => Int -> [a] -> [[a]]
ordered_combinations 1 xs = [[x] | x <- xs]
ordered_combinations n xs = [x:ys | x <- xs, ys <- (ordered_combinations (n-1) (delete x xs))]

combinations :: Eq a => Int -> [a] -> [[a]]
combinations 1 xs = [[x] | x <- xs]
combinations n xs = [x:ys | x <- xs, ys <- (combinations (n-1) (tail $ dropWhile (/=x) xs))]

combination :: Int -> [a] -> [([a],[a])]
combination 0 xs     = [([],xs)]
combination n []     = []
combination n (x:xs) = ts ++ ds
  where
    ts = [ (x:ys,zs) | (ys,zs) <- combination (n-1) xs ]
    ds = [ (ys,x:zs) | (ys,zs) <- combination  n    xs ]
 
group :: [Int] -> [a] -> [[[a]]]
group [] _ = [[]]
group (n:ns) xs =
    [ g:gs | (g,rs) <- combination n xs
           ,  gs    <- group ns rs ]

-- Problem 28
--
-- Sorting a list of lists according to length of sublists
--
-- a) We suppose that a list contains elements that are lists themselves. The objective is to sort the elements of this list according to their length. E.g. short lists first, longer lists later, or vice versa.

lsort :: [[a]] -> [[a]]
lsort [] = []
lsort (x:xs) = 
  lsort (filter (shorterThan x) xs) ++
  [x] ++
  lsort (filter (atLeastAsLongAs x) xs) where
    shorterThan x = (\y -> length y < length x)
    atLeastAsLongAs x = (\y -> length y >= length x)

-- b) Again, we suppose that a list contains elements that are lists themselves. But this time the objective is to sort the elements of this list according to their length frequency; i.e., in the default, where sorting is done ascendingly, lists with rare lengths are placed first, others with a more frequent length come later.

freq :: [a] -> [[a]] -> Int
freq x ys = length $ filter (\y -> length y == length x) ys

lfsort :: [[a]] -> [[a]]
lfsort [] = []
lfsort as@(x:xs) = 
  lfsort (filter (\y -> freq y as < freq x as) xs) ++
  [x] ++
  lfsort (filter (\y -> freq y as >= freq x as) xs)

