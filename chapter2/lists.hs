-- List are homogeneous (All the elements are of the same type)

consOperator :: [Integer]
consOperator = 1 : 2 : 3 : 4 : []

-- head, tail and null are O(1)

append' :: [a] -> [a] -> [a]
append' [] l2 = l2
append' (x : xs) l2 = x : append' xs l2

append'' :: [a] -> [a] -> [a]
append'' = flip $ foldr (:)

-- List comprehension
lRes :: [Integer]
lRes = [x + 1 | x <- [1 .. 10], even x]

-- Tuples are heterogeneous

t1 :: (Integer, String, Float)
t1 = (5, "john", 6.9)