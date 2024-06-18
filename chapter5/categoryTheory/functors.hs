import Test.QuickCheck (Arbitrary, Property, quickCheck, withMaxSuccess, (===))

data Node a = Node a (Maybe (Node a)) (Maybe (Node a)) deriving (Show, Eq)

instance Functor Node where
  fmap :: (a -> b) -> Node a -> Node b
  fmap f (Node x l r) = Node (f x) (fmap f <$> l) (fmap f <$> r)

tree :: Node Int
tree = Node 5 (Just $ Node 2 Nothing (Just $ Node 3 Nothing Nothing)) (Just $ Node 7 Nothing (Just $ Node 9 (Just $ Node 8 Nothing Nothing) Nothing))

newTree :: Node Int
newTree = (+ 1) <$> tree

-- Laws

law1 :: (Functor f, Eq (f a), Show (f a)) => f a -> Property
law1 x = fmap id x === id x

law2 :: (Functor f, Eq (f c), Show (f c)) => (b -> c) -> (a -> b) -> f a -> Property
law2 f g x = fmap (f . g) x === (fmap f . fmap g) x

-- Tests

testLaw1 :: IO ()
testLaw1 = quickCheck . withMaxSuccess 1000 $ (law1 @Maybe @Int)

instance Show (a -> b) where
  show :: (a -> b) -> String
  show _ = "<function>"
  
testLaw2 :: IO ()
testLaw2 = quickCheck . withMaxSuccess 1000 $ (law2 @Maybe @Int @String @Int)

-- Functions as functors

-- fmap f g = f . g

