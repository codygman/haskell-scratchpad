module Krypto
( deal
, solutions
) where

import System.Random(randomRIO)
import Data.List(permutations)
import Text.Show.Functions

-- deal the cards

-- three each of 1-6, four each of 7-10, two each of 11-17, one each of 18-25
deck :: [Int]
deck = ([ 1.. 6] >>= replicate 3)
    ++ ([ 7..10] >>= replicate 4)
    ++ ([11..17] >>= replicate 2)
    ++ ([18..25] >>= replicate 1)

dealOne :: [Int] -> IO ([Int], [Int])
dealOne xs = do
  i <- randomRIO (0, length xs - 1)
  return (xs !! i : [], (take i xs) ++ (drop (i + 1) xs)) 

dealN :: Int -> [Int] -> IO ([Int], [Int])
dealN 1 xs = dealOne xs
dealN n xs = do
  (c1, rest1) <- dealN 1 xs
  (cs, rest2) <- dealN (n - 1) rest1
  return (c1 ++ cs, rest2)

deal :: IO ([Int], Int)
deal = do
  (cards, _) <- dealN 6 deck
  return (drop 1 cards, head cards)

-- solve a deal

opses = sequence $ replicate 4 "+-*/"

toOp :: RealFrac a => Char -> (a -> a -> a)
toOp '+' = (+)
toOp '-' = (-)
toOp '*' = (*)
toOp '/' = (/)

eval :: RealFrac a => [a] -> [Char] -> a
eval [] _ = 0
eval (x : xs) [] = x
eval (x : xs) (opc : ops) = (toOp opc) x (eval xs ops)

solve :: RealFrac a => ([a], a) -> [(a, ([a], [Char]))]
solve (xs, x) = filter ((==) x . fst) [(ans, (nums, ops)) | ops <- opses, nums <- permutations xs, let ans = eval nums ops]

-- show the results

show' :: RealFrac a => [a] -> [Char] -> [Char]
show' [] _ = []
show' (x : xs) [] = show $ floor x
show' (x : xs) (opc : ops)
  | opc == '+' = (show $ floor x) ++ " " ++ [opc] ++ " " ++ (show' xs ops)
  | otherwise  = (show $ floor x) ++ " " ++ [opc] ++ " (" ++ (show' xs ops) ++ ")"

solutions :: RealFrac a => ([a], a) -> [[Char]]
solutions (xs, x) = [(show $ floor x) ++ " = " ++ ans | (_, (nums, ops)) <- solve (xs, x), let ans = show' nums ops]
