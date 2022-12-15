{-# OPTIONS_GHC -fplugin=Dekking #-}

module Lib
  ( covered,
  )
where

import Dekking.Plugin

covered :: IO ()
covered = pure ()

uncovered :: IO ()
uncovered = pure ()
