-- | Module of adapters for values
--
-- Keep this module as small as possible, because it will be imported to adapt
-- values.
module Dekking.ValueLevelAdapter (adaptValue) where

import System.IO
import System.IO.Unsafe

withCoverageHandle :: (Handle -> IO a) -> IO a
withCoverageHandle func = withFile "coverage.dat" AppendMode $ \h -> do
  hSetBuffering h NoBuffering
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
