-- Append is strict on the first argument

-- bottom ++ xs == bottom

-- Space leaks

-- Force evaluation

-- Arguments

forceArgs :: a -> (a, a)
forceArgs x = (x, x)

forceArgsRes :: (Float, Float)
forceArgsRes = forceArgs $! sqrt 52

-- IMPORTANT: f $! x = x `seq` f x

-- Data constructor -> Automatic `seq` for that value

data CustomData = CustomData ![Integer] !Integer

d :: CustomData
d = CustomData [1 .. 10] (1 + 6)

-- seq

foldl' :: (Foldable t1) => (t2 -> t3 -> t2) -> t2 -> t1 t3 -> t2
foldl' f = foldl (\acc x -> acc `seq` f acc x)