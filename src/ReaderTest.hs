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

-- | Reads forms from the input and checks whether the parse succeeded,
-- returning (parseSucceeded XNOR shouldSucceed)
readWithOutcome :: Bool -> String -> Bool
readWithOutcome shouldSucceed input = case readForms "" input of
  Left _ -> not shouldSucceed
  Right _ -> shouldSucceed

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

prop_readString :: String -> Bool
prop_readString s = readAndCheck (show s) (== [Str s])

-- | Represents strings which are *not* valid Cantor syntax
data InvalidSyntax = UnbalancedString String
                     deriving (Show, Read, Eq)

instance QuickCheck.Arbitrary InvalidSyntax where
  arbitrary = QuickCheck.oneof [
    liftM UnbalancedString (balancedString not)
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
  where parens = QuickCheck.listOf $ QuickCheck.elements "()[]{}"

prop_invalidSyntax :: InvalidSyntax -> Bool
prop_invalidSyntax (UnbalancedString s) = readWithOutcome False s

-- | Generate a string that's a valid identifier name.
arbitraryIdent :: QuickCheck.Gen String
arbitraryIdent = do
  c  <- QuickCheck.elements identInitialChars
  cs <- QuickCheck.listOf $ QuickCheck.elements identSubsequentChars
  return $ c:cs

-- | Represents various strings which should parse as valid Cantor syntax
data ValidSyntax = BalancedString String
                 | ArbitraryIdent String
                 | ArbitraryVector String
                 | ArbitraryDict String
                   deriving (Show, Read, Eq)

instance QuickCheck.Arbitrary ValidSyntax where
  arbitrary = QuickCheck.oneof [
    liftM BalancedString (balancedString id),
    liftM ArbitraryIdent arbitraryIdent,
    liftM ArbitraryVector (arbitraryBetween '[' ']'),
    liftM ArbitraryDict (arbitraryBetween '{' '}')
    ]
    
arbitraryBetween :: Char -> Char -> QuickCheck.Gen String
arbitraryBetween c1 c2 = QuickCheck.oneof $ map generate [0 .. 4]
  where generate n = do forms <- QuickCheck.vectorOf n QuickCheck.arbitrary
                        return ([c1] ++ (showForms forms) ++ [c2])

sexpHeadedBy :: Form -> [Form] -> Bool
sexpHeadedBy form ((Sexp (head:_)):_) = head == form
sexpHeadedBy _ _                      = False

prop_validSyntax :: ValidSyntax -> Bool
prop_validSyntax (BalancedString s) =
  readWithOutcome True s
prop_validSyntax (ArbitraryIdent s) =
  readAndCheck s (== [Ident s])
prop_validSyntax (ArbitraryVector s) =
  readAndCheck s (sexpHeadedBy (Ident "vector"))
prop_validSyntax (ArbitraryDict s) =
  readAndCheck s (sexpHeadedBy (Ident "dict"))

readerTests :: Test
readerTests = $testGroupGenerator
