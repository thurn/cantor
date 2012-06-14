-- This is free and unencumbered software released into the public domain.

{-# LANGUAGE TemplateHaskell #-}

module ReaderTest (
  readerTests
) where

import           Control.Monad                        (liftM)
import           Reader
import           Test.Framework                       (Test)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.TH                    (testGroupGenerator)
import qualified Test.QuickCheck                      as QuickCheck

instance QuickCheck.Arbitrary Form where
  arbitrary = QuickCheck.oneof [
    arbitraryAtom,
    arbitrarySexp
    ]

arbitraryAtom :: QuickCheck.Gen Form
arbitraryAtom = QuickCheck.oneof [
  liftM Int QuickCheck.arbitrary,
  liftM Float QuickCheck.arbitrary,
  liftM Str arbitraryString,
  liftM Ident arbitraryIdent
  ]

identInitialChars :: String
identInitialChars = ['a' .. 'z'] ++ ['A' .. 'Z']

identSubsequentChars :: String
identSubsequentChars = identInitialChars ++ "0123456789!%&*-+=\\|?/<>"

-- | Generate a string that's a valid identifier name.
arbitraryIdent :: QuickCheck.Gen String
arbitraryIdent = do
  c  <- QuickCheck.elements identInitialChars
  cs <- QuickCheck.listOf $ QuickCheck.elements identSubsequentChars
  return $ c:cs

arbitraryString :: QuickCheck.Gen String
arbitraryString = do
  s <- QuickCheck.arbitrary
  return $ show (s :: String)

arbitrarySexp :: QuickCheck.Gen Form
arbitrarySexp = QuickCheck.sized randomSexp

randomSexp 0 = arbitraryAtom
randomSexp n = QuickCheck.oneof [
  arbitraryAtom,
  listOfSize 0,
  listOfSize 1,
  listOfSize 2,
  listOfSize 3
  ]
  where nextRandom = randomSexp (n `div` 2)
        listOfSize size = liftM Sexp $ QuickCheck.vectorOf size $ nextRandom

-- | Reads forms from the provided string and applies the provided predicate to
-- the result.
readAndCheck :: String -> ([Form] -> Bool) -> Bool
readAndCheck input predicate = case readForms "" input of
  Left _ -> False
  Right forms -> predicate forms

-- | 'showForms' and 'readForms' are the opposite of each other.
prop_reversible :: [Form] -> Bool
prop_reversible forms = readAndCheck (showForms forms) (== forms)

-- | Any Haskell integer in its standard string representation can be parsed as
-- an Int form.
prop_readInt :: Integer -> Bool
prop_readInt int = readAndCheck (show int) (== [Int int])

-- | Likewise for doubles.
prop_readFloat :: Double -> Bool
prop_readFloat float = readAndCheck (show float) (== [Float float])

newtype ArbitraryIdent = ArbitraryIdent String deriving (Show, Read, Eq)

instance QuickCheck.Arbitrary ArbitraryIdent where
  arbitrary = liftM ArbitraryIdent arbitraryIdent

-- | Generate an arbitrary identifier and try reading it.
prop_readIdent :: ArbitraryIdent -> Bool
prop_readIdent (ArbitraryIdent ident) = readAndCheck ident (== [Ident ident])

prop_readString :: String -> Bool
prop_readString s = readAndCheck (show s) (== [Str s])

readerTests :: Test
readerTests = $testGroupGenerator
