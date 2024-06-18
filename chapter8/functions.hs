import Control.Monad (foldM)

sequence' :: (Monad m) => [m a] -> m [a]
sequence' = foldM (\acc x -> x >>= \e -> return $ e : acc) []

mapM' :: (Monad m) => (a -> m b) -> [a] -> m [b]
mapM' f = sequence . (<$>) f

(=<<*) :: (Monad m) => (a -> m b) -> m a -> m b
(=<<*) = flip (>>=)

foldM' :: (Monad m) => (a -> b -> m a) -> a -> [b] -> m a
foldM' f init = foldl (\acc x -> acc >>= flip f x) (return init)

filterM' :: (Monad m) => (a -> m Bool) -> [a] -> m [a]
filterM' f = foldM (\acc x -> f x >>= g acc x) []
  where
    g acc x True = return $ x : acc
    g acc _ False = return acc

unless' :: (Monad m) => Bool -> m () -> m ()
unless' True = id
unless' False = const $ return ()

when' :: (Monad m) => Bool -> m () -> m ()
when' = unless' . not

liftM' :: (Monad m) => (a -> b) -> (m a -> m b)
liftM' f = (>>= return . f)