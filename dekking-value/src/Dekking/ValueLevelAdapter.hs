-- The docs here:
-- https://hackage.haskell.org/package/base-4.17.0.0/docs/System-IO-Unsafe.html#v:unsafePerformIO
-- recommend these two compiler flags:
-- - Use the compiler flag -fno-cse to prevent common sub-expression elimination being performed on the module, which might combine two side effects that were meant to be separate. A good example is using multiple global variables (like test in the example below).
-- - Make sure that the either you switch off let-floating (-fno-full-laziness), or that the call to unsafePerformIO cannot float outside a lambda. For example, if you say: f x = unsafePerformIO (newIORef []) you may get only one reference cell shared between all calls to f. Better would be f x = unsafePerformIO (newIORef [x]) because now it can't float outside the lambda.
{-# OPTIONS_GHC -fno-cse -fno-full-laziness #-}

-- | Module of adapters for values
--
-- Keep this module as small as possible, because it will be imported to adapt
-- values. Any dependency of this module cannot be code-covered.
module Dekking.ValueLevelAdapter (coverageFileName, adaptValue) where

import System.IO
import System.IO.Unsafe

coverageFileName :: FilePath
coverageFileName = "coverage.dat"

withCoverageHandle :: (Handle -> IO a) -> IO a
withCoverageHandle func =
  withFile coverageFileName AppendMode $ \h -> do
    hSetBuffering h LineBuffering
    func h

{-# NOINLINE adaptValue #-}
adaptValue :: String -> a -> a
adaptValue logStr a = unsafePerformIO $
  withCoverageHandle $ \coverageHandle -> do
    hPutStrLn coverageHandle logStr
    hFlush coverageHandle
    pure a

-- TODO try out 'unsafeDupablePerformIO' and consider whether I
-- need 'unsafeInterleaveIO'
