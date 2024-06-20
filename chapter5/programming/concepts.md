## Lenses

```haskell
type Lens’ s a = forall f. Functor f => (a -> f a) -> s -> f s

newtype Identity a = Identity a

runIdentity :: Identity a -> a
runIdentity (Identity x) = x

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

set :: Lens’ s a -> (a -> s -> s)
-- set ln x s = runIdentity (ln set_fld s)
--     where set_fld _ = Identity x
set ln x = runIdentity . ln (Identity . const x)

newtype Const v a = Const v

getConst :: Const v a -> v
getConst (Const x) = x

instance Functor (Const v) where
    fmap f (Const x) = Const x

view :: Lens’ s a -> (s -> a)
-- view ln s = getConst (ln Const s)
view ln = getConst . ln Const

```

