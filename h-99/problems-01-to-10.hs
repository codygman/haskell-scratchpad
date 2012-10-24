myLast = foldr1 (\a -> (\b -> b))

myButLast (a:b:[]) = a
myButLast (a:bs) = myButLast bs

elementAt (y:ys) x
  | x == 1 = y
  | otherwise = elementAt (ys) (x-1)
 
myLength [] = 0
myLength (x:xs) = 1 + myLength xs

reverse' [] = []
reverse' (x:xs) = reverse' xs ++ [x]

isPalindrome x = x == reverse x

flatten' :: [[a]] -> [a]
flatten' [x] = x
flatten' (x:xs) = x ++ flatten' xs

compress :: Eq a => [a] -> [a]
compress (x:[]) = [x]
compress (x:y:zs)
  | x == y = compress (x:zs)
  | otherwise = x:compress (y:zs)

pack :: Eq a => [a] -> [[a]]
pack = foldr pack' []
    where pack' x [] = [[x]]
          pack' x (y:xs) = if x == (head y)
            then ((x:y):xs)
            else ([x]:y:xs)

encode :: Eq a => [a] -> [(Int,a)]
encode = fmap (\x -> (length x, head x)) . pack

