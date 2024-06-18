import Test.QuickCheck (Arbitrary, Property, quickCheck, withMaxSuccess, (===))

-- Value that can be grouped together (they must be associative)
-- a . (b . c) = (a . b) . c

newtype SumMonoid = SumMonoid Int deriving (Show, Eq, Arbitrary)

instance Semigroup SumMonoid where
  (<>) :: SumMonoid -> SumMonoid -> SumMonoid
  (<>) (SumMonoid x) (SumMonoid y) = SumMonoid $ x + y

instance Monoid SumMonoid where
  mempty :: SumMonoid
  mempty = SumMonoid 0
  mappend :: SumMonoid -> SumMonoid -> SumMonoid
  mappend = (<>)

result1 :: SumMonoid
result1 = foldr (mappend . SumMonoid) mempty [1 .. 100]

result2 :: SumMonoid
result2 = foldMap SumMonoid [1 .. 100]

-- Laws

law1 :: (Monoid a, Eq a, Show a) => a -> Property
law1 x = mempty `mappend` x === x

law2 :: (Monoid a, Eq a, Show a) => a -> Property
law2 x = x `mappend` mempty === x

law3 :: (Monoid a, Eq a, Show a) => a -> a -> a -> Property
law3 x y z = (x `mappend` y) `mappend` z === x `mappend` (y `mappend` z)

-- Tests

testLaw1 :: IO ()
testLaw1 = quickCheck . withMaxSuccess 1000 $ (law1 @SumMonoid)

testLaw2 :: IO ()
testLaw2 = quickCheck . withMaxSuccess 1000 $ (law2 @SumMonoid)

testLaw3 :: IO ()
testLaw3 = quickCheck . withMaxSuccess 1000 $ (law3 @SumMonoid)