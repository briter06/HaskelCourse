import Control.Monad.Error.Class (MonadError (catchError, throwError))

data Either' a b = Left' a | Right' b

instance Functor (Either' a) where
  fmap _ (Left' x) = Left' x
  fmap f (Right' x) = Right' (f x)

instance Applicative (Either' a) where
  pure = Right'
  (<*>) (Left' x) _ = Left' x
  (<*>) (Right' f) (Right' x) = Right' $ f x

instance Monad (Either' a) where
  return = pure
  (Left' e) >>= _ = Left' e
  (Right' x) >>= g = g x

instance MonadError e (Either' e) where
  throwError = Left'
  catchError (Left' msg) f = f msg
  catchError (Right' x) _ = Right' x

-- Example

multiplyBy5 :: Integer -> Either' String Integer
multiplyBy5 x = if x == 5 then throwError "Invalid input" else return (x * 5)

sum1 :: Integer -> Either' String Integer
sum1 x = multiplyBy5 x >>= return . (+ 1)

either' :: (t1 -> t2) -> (t3 -> t2) -> Either' t1 t3 -> t2
either' f _ (Left' x) = f x
either' _ f (Right' x) = f x

result :: Integer
result = either' error id (sum1 6)