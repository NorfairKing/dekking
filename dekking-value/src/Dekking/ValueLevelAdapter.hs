{-# LANGUAGE RankNTypes #-}
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

{-# NOINLINE coverageHandle #-}
coverageHandle :: Handle
coverageHandle = unsafePerformIO $ do
  h <- openFile coverageFileName AppendMode
  hSetBuffering h LineBuffering
  pure h

-- | The value-level adapter function
--
-- The plan is to replace every instance of
--
-- e :: t
--
-- by
--
-- adaptValue "some string that identifies e" e :: t
{-# NOINLINE adaptValue #-}
adaptValue :: String -> (forall a. a -> a)
adaptValue logStr = unsafePerformIO $ do
  hPutStrLn coverageHandle logStr
  hFlush coverageHandle
  pure id

-- TODO try out 'unsafeDupablePerformIO' and consider whether I
-- need 'unsafeInterleaveIO'
