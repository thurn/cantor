-- This is free and unencumbered software released into the public domain.

-- | The Main module for Cantor.
module Main (
    main
) where

import Control.Monad (unless)
import System.Exit (exitFailure)
import qualified Reader

-- | Entry point for unit tests.
main :: IO ()
main = do
    readerTests <- Reader.runTests
    unless (and [readerTests]) exitFailure

