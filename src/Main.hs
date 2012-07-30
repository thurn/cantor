-- This is free and unencumbered software released into the public domain.

{-# LANGUAGE CPP #-}

-- | The Main module for Cantor.
module Main (
    main
) where

import ReaderTest (readerTests)
import TranslatorTest (translatorTests)
import Test.Framework (defaultMain)

-- | Main function
exeMain :: IO ()
exeMain = undefined

-- | Entry point for unit tests
testMain :: IO ()
testMain = defaultMain [
  readerTests,
  translatorTests
 ]

-- This is a clunky, but portable, way to use the same Main module file
-- for both an application and for unit tests.
-- MAIN_FUNCTION is preprocessor macro set to exeMain or testMain.
-- That way we can use the same file for both an application and for tests.
#ifndef MAIN_FUNCTION
#define MAIN_FUNCTION exeMain
#endif
main :: IO ()
main = MAIN_FUNCTION