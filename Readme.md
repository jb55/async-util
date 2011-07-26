
# Control.Async.Util

This package provides some asyncronous higher order utility functions.

This package builds off the async and wait primitives provided by Control.Async,
also described in Simon Marlow's [Parallelism and Concurrency
Tutorial](http://community.haskell.org/~simonmar/par-tutorial.pdf)

# Installation

cabal install

# Functions

## mapA

```haskell
mapA :: (a -> IO b) -> [a] -> IO [b]
```

mapA maps a function over a list asyncronously, spawning each function as an
efficient green thread provided by GHC's runtime system.

Here's an example where we download 3 webpages all at the same time using
Network.HTTP:

```haskell

import Network.HTTP
import Network.Stream
import Control.Async.Util

sites :: [String]
sites = [ "http://google.com"
        , "http://jb55.com"
        , "http://yahoo.com"
        ]

type HttpResult = Result (Response String)

downloadAsync :: IO [HttpResult]
downloadAsync = mapA (simpleHTTP . getRequest) sites

```

`mapA` will download much faster than `mapM` in this case, try switching `mapA`
with `mapM` to compare.

## mapAS

```haskell
mapAS :: Int -> (a -> IO b) -> [a] -> IO [Either AsyncError b]
```

`mapAS` is a much safer version of `mapA`. It allows you to set a timeout for
each call and catches any exceptions/timeouts into Either's Left constructor.

Example:

```haskell

safeDownloadAsync :: IO [Either AsyncError HttpResult]
safeDownloadAsync = mapAS 2000000 (simpleHTTP . getRequest) sites

```

This sets a 2 second timeout (argument is in microseconds) for each IO function,
returning `Left Timeout` if it takes longer. If any exception was thrown during
the async call it is caught as `Left (Exception e)` where e is `SomeException`.
Successful results are returned in Either's `Right` constructor.

# Data Types

## AsyncError

Holds errors for `mapAS`

```haskell

data AsyncError = Timeout
                | Exception SomeException

```
