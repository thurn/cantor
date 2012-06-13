-- This is free and unencumbered software released into the public domain.

{-# LANGUAGE TemplateHaskell #-}

module ReaderTest where

import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.Framework.TH (testGroupGenerator)
import Test.HUnit ((@?=))

readerTests = $testGroupGenerator

case_1 = do 1 @?= 12

case_2 = do 2 @?= 2

prop_reverse xs = reverse (reverse xs) == xs
    where types = xs::[Int]
