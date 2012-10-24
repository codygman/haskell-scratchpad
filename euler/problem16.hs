-- What is the sum of the digits of the number 2^1000?

import Data.Char(digitToInt)

problem16 = sum $ fmap digitToInt . show $ 2^1000
