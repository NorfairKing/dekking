module TopLevel where

main :: IO ()
main = do
  covered
  coveredWithArg 5

covered :: IO ()
covered = pure ()

coveredWithArg :: Int -> IO ()
coveredWithArg _ = pure ()

uncovered :: IO ()
uncovered = pure ()

uncoveredWithArg :: Int -> IO ()
uncoveredWithArg _ = pure ()
