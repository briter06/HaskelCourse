-- Arithmetic expressions
expr :: Integer
expr = 50 * 100 - 100

-- Booleans
bool1 :: Bool
bool1 = not (True && True)

-- Function precedence
-- Function application has the highest precedence
maxRes :: Integer
maxRes = max 5 6 + min 4 9

-- Type declarations are optional but must be correct

-- Everthing in haskell takes only one argument

-- Float vs Double

-- Single precision (32 bits)
doublePiFloat :: Float
doublePiFloat = pi * 2

-- Double precision (64 bits)
doublePiDouble :: Double
doublePiDouble = pi * 2

-- Local binding
letRes :: Integer
letRes = let x = 5 in x + 1

whereRes :: Integer
whereRes = x + 1
  where
    x = 5

-- Pattern match for atomic types
greet :: String -> String
greet "John" = "Invalid name"
greet name = "Hello " ++ name

-- Universal polymorphism
first :: a -> b -> a
first x y = x

-- Bounded polymorphism
areEqual :: (Eq a) => a -> a -> Bool
areEqual x y = x == y

getOrdering :: (Ord a) => a -> a -> Ordering
getOrdering = compare

minBoundCustom :: (Bounded a) => a
minBoundCustom = minBound

-- Functions with multiple parameters are called "curried" functions
-- Pointfree programming
multiplyBy10 :: Integer -> Integer
multiplyBy10 = (* 10)

-- Lambdas
lambda :: Integer -> Integer
lambda = \x -> x + 1

-- Function composition
applyTwice :: (a -> a) -> a -> a
applyTwice f = f . f

