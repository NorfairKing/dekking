module Annotations where

-- See [ref:DisablingCoverage]
{-# ANN module "NOCOVER" #-}

{-# ANN main "NOCOVER" #-}
main :: IO ()
main = pure ()
