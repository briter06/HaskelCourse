import Control.Monad.Reader.Class (MonadReader (ask, local))

newtype Reader e a = Reader {runReader :: e -> a}

instance Functor (Reader e) where
  fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader e) where
  pure = Reader . const
  (<*>) (Reader f) (Reader g) = Reader $ \e -> f e (g e)

instance Monad (Reader e) where
  return = pure
  (Reader f) >>= g = Reader $ \e -> runReader (g (f e)) e -- A reader than combines two actions with the same environment

instance MonadReader e (Reader e) where
  ask = Reader id -- A reader that returns the environment
  local f c = Reader $ \e -> runReader c (f e) -- Runs a reader with a modified environment

-- Example

data Exp = Ref String | Num Integer | Add Exp Exp | Let String Exp Exp

type Env = String -> Maybe Integer

empty :: Env
empty _ = Nothing

addKey :: String -> Integer -> Env -> Env
addKey key value env k = if key == k then Just value else env key

eval :: Exp -> Reader Env Integer
eval (Let key value exp) = eval value >>= \r -> local (addKey key r) (eval exp)
eval (Ref key) = ask >>= \env -> maybe (error "Variable doesn't exist") return $ env key
eval (Num x) = return x
eval (Add exp1 exp2) = do
  v1 <- eval exp1
  v2 <- eval exp2
  return (v1 + v2)

testExp :: Exp
testExp = Let "v" (Num 1) $ Add (Ref "v") (Num 5)

result :: Integer
result = runReader (eval testExp) empty