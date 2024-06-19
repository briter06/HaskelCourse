import Control.Concurrent (MVar, forkIO, newEmptyMVar, putMVar, takeMVar)
import Control.Parallel (pseq)

fib :: Int -> Int
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

fibThread :: Int -> MVar Int -> IO ()
fibThread n resultMVar = do
  f `pseq` return ()
  putMVar resultMVar f
  where
    f = fib n

re1 :: Int
re1 = fib 40

main :: IO ()
main = do
  fibResult <- newEmptyMVar
  forkIO $ fibThread 40 fibResult
  pseq re1 (return ())
  f <- takeMVar fibResult
  print (re1 + f)