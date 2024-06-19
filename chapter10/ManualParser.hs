import Data.Char (isLower)
import Test.QuickCheck (Arbitrary, Property, quickCheck, withMaxSuccess, (===))

newtype Parser a = MkP (String -> [(a, String)])

instance Functor Parser where
  fmap f (MkP g) = MkP $ \s -> map (\(a, newS) -> (f a, newS)) (g s)

instance Applicative Parser where
  pure a = MkP $ \s -> [(a, s)]
  (MkP f) <*> (MkP g) = MkP $ \s -> fmap (\x -> (x, s)) $ fmap fst (f s) <*> fmap fst (g s)

instance Monad Parser where
  return = pure
  p >>= q = MkP $ \s -> [(y, s'') | (x, s') <- apply p s, (y, s'') <- apply (q x) s']

apply :: Parser a -> String -> [(a, String)]
apply (MkP f) = f

-- Laws

-- law1 :: (Eq (Parser b), Show (Parser b)) => (a -> Parser b) -> Property
-- law1 p = (zero >>= p) === zero

-- law2 :: (Eq (Parser a), Show (Parser a)) => Parser a -> Property
-- law2 p = (p >>= const zero) === zero

-- Basic parsers

item :: Parser Char
item = MkP f
  where
    f [] = []
    f (c : cs) = [(c, cs)]

zero :: Parser a
zero = MkP $ const []

sat :: (Char -> Bool) -> Parser Char
sat p = do
  c <- item
  if p c then return c else zero

lower :: Parser Char
lower = sat isLower

plus :: Parser a -> Parser a -> Parser a
p `plus` q = MkP $ \s -> apply p s ++ apply q s

orelse :: Parser a -> Parser a -> Parser a
p `orelse` q = MkP $ \s -> if null (apply p s) then apply q s else apply p s

lowers :: Parser String
lowers =
  do
    c <- lower
    cs <- lowers
    return (c : cs)
    `orelse` return ""