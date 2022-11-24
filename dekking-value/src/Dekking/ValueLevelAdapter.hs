-- | Module of adapters for values
--
-- Keep this module as small as possible, because it will be imported to adapt
-- values. Any dependency of this module cannot be code-covered.
module Dekking.ValueLevelAdapter (coverageFileName, adaptValue) where

import Control.Exception
import GHC.IO.Handle.Lock (LockMode (ExclusiveLock), hLock, hUnlock)
import System.IO
import System.IO.Unsafe

coverageFileName :: FilePath
coverageFileName = "coverage.dat"

withCoverageHandle :: (Handle -> IO a) -> IO a
withCoverageHandle func =
  withFile coverageFileName AppendMode $ \h ->
    bracket_ (hLock h ExclusiveLock) (hUnlock h) $ do
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
