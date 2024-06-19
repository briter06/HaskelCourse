# Monads

Monads are values with an issue, and the offer a way to inspect and manipulate that value

## Monad class

```haskell
class Applicative m => Monad m where
  (>>=) :: m a -> (a -> m b) -> m b
  (>>) :: m a -> m b -> m b
  return :: a -> m a
```

### Laws

```haskell
1. return x >>= f === f x
2. m >>= return === m
3. (m >>= f) >>= g === m >>= (\x -> f x >>= g)
```

## MonadPlus class

Monads that are also monoids

```haskell
class (Monad m) => MonadPlus m where
  mzero :: m a
  mplus :: m a -> m a -> m a
```

## Different monads

### List monad

```haskell
instance Functor [] where
    fmap = map

instance Applicative [] where
    pure x = [x]
    fs <*> xs = [f x | f <- fs, x <- xs]

instance Monad [] where
    return x = [x]
    xs >>= f = concat (map f xs)

instance MonadPlus [] where
    mzero = []
    mplus = (++)
```

### Maybe monad

```haskell
instance Functor Maybe where
    fmap _ Nothing  = Nothing
    fmap f (Just x) = Just (f x)

instance Applicative Maybe where
    pure = Just
    Nothing <*> _ = Nothing
    (Just f) <*> something = fmap f something

instance Monad Maybe where
    return = Just
    Nothing >>= _ = Nothing
    (Just x) >>= f = f x
```
