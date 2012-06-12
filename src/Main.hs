-- This is free and unencumbered software released into the public domain.

{-# LANGUAGE CPP #-}

-- | The Main module for Cantor.
module Main (
    main
) where

import Control.Monad (unless)
import System.Exit (exitFailure)
import qualified Reader

-- Hello World
exeMain :: IO ()
exeMain = putStrLn "Hello, World"

-- Entry point for unit tests.
testMain :: IO ()
testMain = do
    readerTests <- Reader.runTests
    unless (and [readerTests]) exitFailure

#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif

-- | Either runs the main function or runs quickCheck tests
main :: IO ()
main = MAIN_FUNCTION
