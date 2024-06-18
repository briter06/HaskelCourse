import Control.Monad.Writer.Class (MonadWriter (listen, pass, tell))

newtype Writer w a = Writer {runWriter :: (a, w)}

instance Functor (Writer w) where
  fmap f (Writer (a, w)) = Writer (f a, w)

instance (Monoid w) => Applicative (Writer w) where
  pure a = Writer (a, mempty)
  (<*>) :: (Monoid w) => Writer w (a -> b) -> Writer w a -> Writer w b
  (<*>) (Writer (f, _)) (Writer (a, w)) = Writer (f a, w)

instance (Monoid w) => Monad (Writer w) where
  return = pure
  (Writer (a, w)) >>= g = let (newA, newW) = runWriter (g a) in Writer (newA, w `mappend` newW)

instance (Monoid w) => MonadWriter w (Writer w) where
  tell w = Writer ((), w) -- Add to the log
  listen ma = let (a, w) = runWriter ma in Writer ((a, w), w) -- Read the log
  pass ma = let ((a, f), w) = runWriter ma in Writer (a, f w) -- Transform the log

seqFromTo :: Integer -> Integer -> Writer [String] Integer
seqFromTo from to
  | from == to = return to
  | otherwise = do
      tell ["Processing " ++ show from]
      seqFromTo (from + 1) to

result :: (Integer, [String])
result = runWriter (seqFromTo 1 20)