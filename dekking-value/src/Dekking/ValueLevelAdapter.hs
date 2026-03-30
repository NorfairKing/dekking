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

-- [tag:ThePlanTM]
--
-- The plan is to replace every instance of
--
-- e :: t
--
-- by
--
-- _dekking_<line>_<colStart>_<colEnd> e :: t
--
-- where _dekking_<line>_<colStart>_<colEnd> is a top-level NOINLINE CAF:
--
-- {-# NOINLINE _dekking_<line>_<colStart>_<colEnd> #-}
-- _dekking_<line>_<colStart>_<colEnd> :: forall a. a -> a
-- _dekking_<line>_<colStart>_<colEnd> = adaptValue "some string that identifies e"
--
-- Because the partial application is a top-level CAF, the unsafePerformIO
-- inside adaptValue fires exactly once per source location (even at -O0),
-- and is then updated to 'id' by GHC's thunk update mechanism.
--
-- This involves adding an import of this module to every source-transformed
-- module.

-- | The value-level adapter function
--
-- Records that an expression at the given source location was evaluated.
-- Each call site is a top-level CAF (see [ref:ThePlanTM]), so the
-- unsafePerformIO runs exactly once per source location. Blackholing
-- is safe here because there are no circular dependencies between CAFs
-- (each just writes a line to the coverage handle).
--
-- We use unsafePerformIO (not unsafeDupablePerformIO) because the
-- latter can cause "thread blocked indefinitely in an MVar operation"
-- errors: duplicate evaluations of the same CAF both call hPutStrLn
-- on the shared Handle, and if one is killed mid-write the Handle's
-- internal MVar is never released.
{-# NOINLINE adaptValue #-}
adaptValue :: String -> (forall a. a -> a)
adaptValue logStr = unsafePerformIO $ do
  hPutStrLn coverageHandle logStr
  pure id
