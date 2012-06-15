-- This is free and unencumbered software released into the public domain.

{-# LANGUAGE TemplateHaskell #-}

module ReaderTest (
  readerTests
) where

import           Control.Monad                        (liftM)
import           Data.List                            (foldl')
import           Reader
import           Test.Framework                       (Test)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.TH                    (testGroupGenerator)
import qualified Test.QuickCheck                      as QuickCheck

instance QuickCheck.Arbitrary Form where
  arbitrary = QuickCheck.sized arbitraryForm

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

arbitraryForm :: Int -> QuickCheck.Gen Form
arbitraryForm 0 = arbitraryAtom
arbitraryForm n = QuickCheck.frequency [
  (15, arbitraryAtom),
  (10, listOfSize 1),
  (5, listOfSize 2),
  (1, listOfSize 3),
  (1, listOfSize 4),
  (1, listOfSize 5),
  (1, listOfSize 0)
  ]
  where nextRandom = arbitraryForm (n `div` 2)
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

-- | Represents strings which are *not* valid Cantor syntax
newtype InvalidSyntax = InvalidSyntax String deriving (Show, Read, Eq)

instance QuickCheck.Arbitrary InvalidSyntax where
  arbitrary = QuickCheck.oneof [
    liftM InvalidSyntax (balancedString not)
    ]

-- | Checks if a string consists of balanced parentheses
isBalanced :: String -> Bool
isBalanced list = null $ foldl' op [] list
  where op ('(':xs) ')' = xs
        op ('[':xs) ']' = xs
        op ('{':xs) '}' = xs
        op xs x         = x:xs

-- | Generates strings consisting of either balanced parentheses (by passing
-- 'id' as the predicate) or unbalanced parentheses (by passing 'not')
balancedString :: (Bool -> Bool) -> QuickCheck.Gen String
balancedString p = QuickCheck.suchThat parens (p . isBalanced)
  where parens = QuickCheck.listOf $ QuickCheck.elements "()"

prop_invalidSyntax :: InvalidSyntax -> Bool
prop_invalidSyntax (InvalidSyntax s) = case readForms "" s of
  Left _ -> True
  Right _ -> False

-- | Represents strings which should parse as valid Cantor syntax
newtype ValidSyntax = ValidSyntax String deriving (Show, Read, Eq)

instance QuickCheck.Arbitrary ValidSyntax where
  arbitrary = QuickCheck.oneof [
    liftM ValidSyntax (balancedString id)
    ]

prop_validSyntax :: ValidSyntax -> Bool
prop_validSyntax (ValidSyntax s) = case readForms "" s of
  Left _ -> False
  Right _ -> True

readerTests :: Test
readerTests = $testGroupGenerator
