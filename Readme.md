
# Control.Async.Util

This package provides some asyncronous higher order utility functions.

This package builds off the async and wait primitives provided by Control.Async,
also described in Simon Marlow's [Parallelism and Concurrency
Tutorial](http://community.haskell.org/~simonmar/par-tutorial.pdf)

# Installation

cabal install

# Functions

## amap

```haskell
amap :: (a -> IO b) -> [a] -> IO [b]
```

amap maps a function over a list asyncronously, spawning each function as an
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
downloadAsync = amap (simpleHTTP . getRequest) sites

```

`amap` will download much faster than `mapM` in this case, try switching `amap`
with `mapM` to compare.

## amapS

```haskell
amapS :: Int -> (a -> IO b) -> [a] -> IO [Either AsyncError b]
```

`amapS` is a much safer version of `amap`. It allows you to set a timeout for
each call and catches any exceptions/timeouts into Either's Left constructor.

Example:

```haskell

safeDownloadAsync :: IO [Either AsyncError HttpResult]
safeDownloadAsync = amapS 2000000 (simpleHTTP . getRequest) sites

```

This sets a 2 second timeout (argument is in microseconds) for each IO function,
returning `Left Timeout` if it takes longer. If any exception was thrown during
the async call it is caught as `Left (Exception e)` where e is `SomeException`.
Successful results are returned in Either's `Right` constructor.

# Data Types

## AsyncError

Holds errors for `amapS`

```haskell

data AsyncError = Timeout
                | Exception SomeException

```
