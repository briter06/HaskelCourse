## Threads

* The thunk must be forced (pseq)
* It's complicated and similar to imperative programming
* Pattern Rendez-Vous -> Communication using MVars

```haskell
forkIO :: IO () -> IO ThreadId
data MVar a = ...
newEmptyMVar :: IO (MVar a)
newMVar :: a -> IO (MVar a)

-- Blocking
takeMVar :: MVar a -> IO a
putMVar :: MVar a -> a -> IO ()
readMVar :: MVar a -> IO a

-- Non-blocking
tryTakeMVar :: MVar a -> IO (Maybe a)
tryPutMVar :: MVar a -> a -> IO Bool
isEmptyMVar :: MVar a -> IO Bool
```

## STM

* TVars can be only used inside an STM ()
* atomically is of type :: STM a -> IO a
* A transaction can't have IO actions because it could be executed more than once (it should only use pure functions)
* A transaction writes are done in a thread-local log. And commits only if successful
* Only shared memory. Not message passing

## STM Monad

```haskell
data STM a = ...
retry :: STM a -- Retries a transaction
orElse :: STM a -> STM a -> STM a -- Executes the second transaction if first one fails
```