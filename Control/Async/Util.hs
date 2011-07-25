{-# LANGUAGE ScopedTypeVariables #-}

module Control.Async.Util ( amap
                          , amapS
                          , AsyncError(..)
                          ) where
  
import Control.Async (waitForAsync, forkAsync, Async(..))
import System.Timeout (timeout)
import Control.Exception

data AsyncError = Timeout
                | Exception SomeException

instance Show AsyncError where
  show Timeout       = "Timeout"
  show (Exception e) = "Exception '" ++ show e ++ "'"

async :: IO a -> IO (Async a)
async = forkAsync

wait :: Async a -> IO a
wait = waitForAsync

-- | Map an IO-producing function over a list, executing each one asyncronously.
--   Timeouts and exceptions are not handled by this function, so use it
--   carefully
amap :: (a -> IO b) -> [a] -> IO [b]
amap f xs = mapM (async . f) xs >>= mapM wait

-- | Very safe version of amap. The first argument is the timeout in
--   microseconds. Any timeouts or exceptions are returned as a Left constructor
--   holding the error that occured. Successful results are held in the Right
--   constructor
amapS :: Int -> (a -> IO b) -> [a] -> IO [Either AsyncError b]
amapS n f xs = amap (timeout n . try . f) xs >>= return . map conv
  where
    conv Nothing                          = Left Timeout
    conv (Just (Left (e::SomeException))) = Left (Exception e)
    conv (Just (Right r))                 = Right r
