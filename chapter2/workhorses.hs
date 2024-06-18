-- Workhorse 1 -> Map / filter

map' :: (Foldable t) => (a -> b) -> t a -> [b]
map' f = foldr ((:) . f) []

filter' :: (Foldable t1) => (a -> Bool) -> t1 a -> [a]
filter' f = foldr g []
  where
    g x acc
      | f x = x : acc
      | otherwise = acc

-- Workhorse 2 -> Zip / ZipWith

zip' :: [a] -> [b] -> [(a, b)]
zip' [] _ = []
zip' _ [] = []
zip' (x : xs) (y : ys) = (x, y) : zip' xs ys

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x : xs) (y : ys) = f x y : zipWith' f xs ys

-- Workhorse 3 -> Flip

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

-- Workhorse 4 -> fold

foldr' :: (a -> b -> b) -> b -> [a] -> b
foldr' _ acc [] = acc
foldr' f acc (x : xs) = f x $ foldr' f acc xs

foldl' :: (b -> a -> b) -> b -> [a] -> b
foldl' _ acc [] = acc
foldl' f acc (x : xs) = foldl' f (f acc x) xs

-- Workhorse 5 -> Scans

scanr' :: (a -> b -> b) -> b -> [a] -> [b]
scanr' f initial = reverse . foldr (\x acc@(a : _) -> f x a : acc) [initial]

scanl' :: (b -> a -> b) -> b -> [a] -> [b]
scanl' f initial = reverse . foldl (\acc@(a : _) x -> f a x : acc) [initial]

-- Foldr
tail' :: [a] -> [a]
tail' = snd . foldr (\x (xs, _) -> (x : xs, xs)) ([], [])