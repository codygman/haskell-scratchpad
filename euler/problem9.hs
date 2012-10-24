{-
 - A Pythagorean triplet is a set of three natural numbers, a < b < c, for which,
 - a2 + b2 = c2
 -
 - For example, 32 + 42 = 9 + 16 = 25 = 52.
 -
 - There exists exactly one Pythagorean triplet for which a + b + c = 1000.
 - Find the product abc.
 -}

problem9 = head [a * b * c | a <- [1..998], b <- [a..999], let c = 1000 - b - a, a + b + c == 1000, a*a + b*b == c*c]

