-- If all the numbers from 1 to 1000 (one thousand) inclusive were written out in words, how many letters would be used?

ones :: Char -> Int
ones '0' = 0 -- zero
ones '1' = 3 -- one
ones '2' = 3 -- two
ones '3' = 5 -- three
ones '4' = 4 -- four
ones '5' = 4 -- five
ones '6' = 3 -- six
ones '7' = 5 -- seven
ones '8' = 5 -- eight
ones '9' = 4 -- nine

teens :: Char -> Int
teens '0' = 3 -- ten
teens '1' = 6 -- eleven
teens '2' = 6 -- twelve
teens '3' = 8 -- thirteen
teens '4' = 8 -- fourteen
teens '5' = 7 -- fifteen
teens '6' = 7 -- sixteen
teens '7' = 9 -- seventeen
teens '8' = 8 -- eighteen
teens '9' = 8 -- nineteen

tens :: [Char] -> Int
tens ("00")     = 0
tens ('0':x:[]) = ones x -- x
tens ('1':x:[]) = teens x -- x
tens ('2':x:[]) = 6 + ones x -- twenty-x
tens ('3':x:[]) = 6 + ones x -- thirty-x
tens ('4':x:[]) = 5 + ones x -- forty-x
tens ('5':x:[]) = 5 + ones x -- fifty-x
tens ('6':x:[]) = 5 + ones x -- sixty-x
tens ('7':x:[]) = 7 + ones x -- seventy-x
tens ('8':x:[]) = 6 + ones x -- eighty-x
tens ('9':x:[]) = 6 + ones x -- ninety-x
tens _          = 0

hundreds :: [Char] -> Int
hundreds ('0' : xs) = tens xs
hundreds (x : "00") = (ones x) + 7 -- x hundred
hundreds (x : xs)   = (ones x) + 7 + 3 + (tens xs) -- x hundred and xs

count :: [Char] -> Int
count (a : [])             = ones a
count (a : b : [])         = tens [a,b]
count (a : b : c : [])     = hundreds [a,b,c]
count (a : b : c : d : []) = (ones a) + 8 + hundreds (b:c:d:[]) -- x thousand x

problem17 = sum $ fmap count $ fmap show [1..1000] -- 21124
