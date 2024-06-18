import Control.Monad.State.Class (MonadState (get, put))

newtype State s a = State {runState :: s -> (a, s)}

instance Functor (State s) where
  fmap f (State g) = State $ \s -> let (a, pS) = g s in (f a, pS)

instance Applicative (State s) where
  pure a = State $ \s -> (a, s)
  (<*>) (State f) (State g) = State $ \s ->
    let (fm, s') = f s
        (a, pS) = g s'
     in (fm a, pS)

instance Monad (State s) where
  return = pure
  (State f) >>= g = State $ \s -> let (a, s') = f s in runState (g a) s'

instance MonadState s (State s) where
  get = State $ \s -> (s, s)
  put newS = State $ const ((), newS)

-- Example

type Counter = State (Integer, Integer)

increment :: Counter ()
increment = get >>= \(counter, factor) -> put (counter + factor, factor * 2)

getCounter :: Counter Integer
getCounter = get >>= \(counter, _) -> return counter

computation :: Counter Integer
computation = increment >> increment >> increment >> increment >> getCounter

result :: Integer
result = fst . runState computation $ (0, 1)