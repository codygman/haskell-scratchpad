-- What is the millionth lexicographic permutation of the digits 0, 1, 2, 3, 4, 5, 6, 7, 8 and 9?

problem24 = (sort $ permutations "0123456789") !! 999999
