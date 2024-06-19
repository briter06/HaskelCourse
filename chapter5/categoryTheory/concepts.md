## Functor class

```haskell
class Functor f where
  fmap :: (a -> b) -> f a -> f b
  (<$>) :: (a -> b) -> f a -> f b
  (<$) :: a -> f b -> f a
```

### Laws

```haskell
1. fmap id x === id x
2. fmap (f . g) x === (fmap f . fmap g) x
```

### Functions as functors

```haskell
fmap = (.)
fmap f g = f . g
fmap f g = \x -> f (g x)
```

## Applicative class

```haskell
class Functor f => Applicative f where
  pure :: a -> f a
  (<*>) :: f (a -> b) -> f a -> f b
  liftA2 :: (a -> b -> c) -> f a -> f b -> f c
  (*>) :: f a -> f b -> f b
  (<*) :: f a -> f b -> f a
```

### Laws

```haskell
1. (pure id <*> v) === v
2. (pure f <*> pure v) === pure (f v)
3. (u <*> pure y) === (pure ($ y) <*> u)
4. (pure (.) <*> u <*> v <*> w) === (u <*> (v <*> w))
```

### Functions as applicatives

```haskell
pure a = \_ -> a
f <*> g = \x -> f x (g x)
```