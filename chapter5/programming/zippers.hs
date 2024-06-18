-- Keep breadcrumbs to traverse a data structure

data Node a = Node a (Maybe (Node a)) (Maybe (Node a)) deriving (Show)

data Breadcrumb a = L (a, Maybe (Node a)) | R (a, Maybe (Node a)) deriving (Show)

type TreeZipper a = (Node a, [Breadcrumb a])

goLeft :: TreeZipper a -> TreeZipper a
goLeft (Node x (Just l) r, bcs) = (l, L (x, r) : bcs)

goRight :: TreeZipper a -> TreeZipper a
goRight (Node x l (Just r), bcs) = (r, R (x, l) : bcs)

goBack :: TreeZipper a -> TreeZipper a
goBack (node, L (x, r) : bcs) = (Node x (Just node) r, bcs)
goBack (node, R (x, l) : bcs) = (Node x l (Just node), bcs)

tree :: Node Int
tree = Node 5 (Just $ Node 2 Nothing (Just $ Node 3 Nothing Nothing)) (Just $ Node 7 Nothing (Just $ Node 9 (Just $ Node 8 Nothing Nothing) Nothing))

treeZipper :: TreeZipper Int
treeZipper = (tree, [])

result :: TreeZipper Int
result = goBack . goBack . goBack . goLeft . goRight . goRight $ treeZipper