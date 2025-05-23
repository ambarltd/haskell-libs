module Util.Directory where

import System.IO.Extra (withTempFile)
import System.Directory (copyFile)

-- | Write to a file such that either the write succeeds or it doesn't happen.
-- The file would never be corrupted by being interrupted halfway through
-- writing.
writeAtomically
  :: FilePath           -- ^ destination to write to
  -> (FilePath -> IO a) -- ^ write to it using the given path
  -> IO a
writeAtomically dst f =
  withTempFile $ \tmp -> do
    r <- f tmp
    copyFile tmp dst
    return r
