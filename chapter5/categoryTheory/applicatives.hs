import Test.QuickCheck (Arbitrary, Property, quickCheck, withMaxSuccess, (===))

data Node a = Node a (Maybe (Node a)) (Maybe (Node a)) deriving (Show, Eq)

instance Functor Node where
  fmap :: (a -> b) -> Node a -> Node b
  fmap f (Node x l r) = Node (f x) (fmap f <$> l) (fmap f <$> r)

instance Applicative Node where
  pure :: a -> Node a
  pure x = Node x Nothing Nothing
  (<*>) :: Node (a -> b) -> Node a -> Node b
  (<*>) (Node f lf rf) (Node x lx rx) = Node (f x) ((<*>) <$> lf <*> lx) ((<*>) <$> rf <*> rx)

tree :: Node Int
tree = Node 5 (Just $ Node 2 Nothing (Just $ Node 3 Nothing Nothing)) (Just $ Node 7 Nothing (Just $ Node 9 (Just $ Node 8 Nothing Nothing) Nothing))

newTree :: Node Int
newTree = (+) <$> tree <*> tree

-- Laws

law1 :: (Applicative f, Eq (f a), Show (f a)) => f a -> Property
law1 v = (pure id <*> v) === v

law2 :: (Eq b, Show b) => (a -> b) -> a -> Property
law2 f v = (cPure f <*> pure v) === cPure (f v)
  where
    cPure = pure :: a -> [a]

law3 :: (Applicative f, Eq (f b), Show (f b)) => f (a -> b) -> a -> Property
law3 u y = (u <*> pure y) === (pure ($ y) <*> u)

law4 :: (Applicative f, Eq (f c), Show (f c)) => f (b -> c) -> f (a -> b) -> f a -> Property
law4 u v w = (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w))

-- Tests

instance Show (a -> b) where
  show :: (a -> b) -> String
  show _ = "<function>"

testLaw1 :: IO ()
testLaw1 = quickCheck . withMaxSuccess 1000 $ (law1 @Maybe @Int)

testLaw2 :: IO ()
testLaw2 = quickCheck . withMaxSuccess 1000 $ (law2 @Int @Int)

testLaw3 :: IO ()
testLaw3 = quickCheck . withMaxSuccess 1000 $ (law3 @Maybe @Int @Int)

testLaw4 :: IO ()
testLaw4 = quickCheck . withMaxSuccess 1000 $ (law4 @Maybe @Int @Int @Int)

-- Functions as applicative functors

-- f <*> g = \x -> f x (g x)