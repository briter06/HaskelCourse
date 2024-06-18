import Control.Monad (when)
import Control.Monad.Cont.Class (MonadCont (callCC))

newtype Cont r a = Cont {runCont :: (a -> r) -> r}

instance Functor (Cont r) where
  fmap f (Cont g) = Cont $ \h -> g (h . f)

instance Applicative (Cont r) where
  pure a = Cont $ \h -> h a
  (Cont f) <*> (Cont g) = Cont $ \h -> f (\t -> g (h . t))

instance Monad (Cont r) where
  return = pure
  (Cont c) >>= f = Cont $ \h -> c (\a -> runCont (f a) h)

instance MonadCont (Cont r) where
  callCC f = Cont $ \h -> runCont (f (Cont . const . h)) h

validateName :: (Applicative f, Foldable t) => t a -> (String -> f ()) -> f ()
validateName name exit = do
  when (null name) (exit "You forgot to tell me your name!")

whatsYourName :: String -> String
whatsYourName name = (`runCont` id) $ do
  callCC $ \exit -> do
    validateName name exit
    return $ "Welcome " ++ name ++ "!"

result :: String
result = whatsYourName "Briter"