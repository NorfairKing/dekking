-- | Module of adapters for values
--
-- Keep this module as small as possible, because it will be imported to adapt
-- values.
module Dekking.ValueLevelAdapter (adaptValue) where

import System.IO
import System.IO.Unsafe

-- Global handle on the coverage file
{-# NOINLINE coverageHandle #-}
coverageHandle :: Handle
coverageHandle = unsafePerformIO $ do
  h <- openFile "coverage.dat" AppendMode
  hSetBuffering h NoBuffering
  pure h

{-# NOINLINE adaptValue #-}
adaptValue :: String -> a -> a
adaptValue logStr a = unsafePerformIO $ do
  hPutStrLn coverageHandle logStr
  hFlush coverageHandle
  pure a

-- TODO try out 'unsafeDupablePerformIO' and consider whether I
-- need 'unsafeInterleaveIO'
