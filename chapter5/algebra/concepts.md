## Semigroup class

```haskell
class Semigroup a where
  (<>) :: a -> a -> a
```

## Monoid Class

```haskell
class Semigroup a => Monoid a where
  mempty :: a
  mappend :: a -> a -> a -- Usually mappend = (<>)
  mconcat :: [a] -> a
```
