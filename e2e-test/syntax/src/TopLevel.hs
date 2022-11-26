module TopLevel where

covered :: IO ()
covered = pure ()

coveredWithArg :: Int -> IO ()
coveredWithArg _ = pure ()

uncovered :: IO ()
uncovered = pure ()

uncoveredWithArg :: Int -> IO ()
uncoveredWithArg _ = pure ()
