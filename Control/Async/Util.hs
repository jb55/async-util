{-# LANGUAGE ScopedTypeVariables #-}

module Control.Async.Util ( mapA
                          , mapAS
                          , liftS
                          , liftST
                          , AsyncError(..)
                          ) where
  
import Control.Async (waitForAsync, forkAsync, Async(..))
import System.Timeout (timeout)
import Control.Exception
import Control.Monad

data AsyncError = Timeout
                | Exception SomeException

instance Show AsyncError where
  show Timeout       = "Timeout"
  show (Exception e) = "Exception '" ++ show e ++ "'"

async :: IO a -> IO (Async a)
async = forkAsync

wait :: Async a -> IO a
wait = waitForAsync


mapAsync :: (a -> IO b) -> [a] -> IO [Async b]
mapAsync f xs = mapM (async . f) xs

-- | Map an IO-producing function over a list, executing each one asyncronously.
--   Timeouts and exceptions are not handled by this function, so use it
--   carefully
mapA :: (a -> IO b) -> [a] -> IO [b]
mapA f xs = mapAsync f xs >>= mapM wait


mapA_ :: (a -> IO b) -> [a] -> IO ()
mapA_ f xs = mapAsync f xs >>= mapM_ wait


-- | Very safe version of mapA. The first argument is the timeout in
--   microseconds. Any timeouts or exceptions are returned as a Left constructor
--   holding the error that occured. Successful results are held in the Right
--   constructor.
mapAS :: Int -> (a -> IO b) -> [a] -> IO [Either AsyncError b]
mapAS n f xs = mapA (liftST n f) xs


liftS :: (a -> IO b) -> a -> IO (Either SomeException b)
liftS f = try . f


liftST :: Int -> (a -> IO b) -> a -> IO (Either AsyncError b)
liftST n f = liftM conv . try . timeout n . f
  where
    conv :: Either SomeException (Maybe a) -> Either AsyncError a
    conv (Left e)         = Left (Exception e)
    conv (Right Nothing)  = Left Timeout
    conv (Right (Just r)) = Right r

