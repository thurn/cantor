-- This is free and unencumbered software released into the public domain.

{-# LANGUAGE TemplateHaskell #-}

module ReaderTest (
  readerTests
) where

import           Control.Monad                        (liftM)
import           Data.List                            (foldl')
import           Reader
import           Test.Framework                       (Test)
import           Test.Framework.Providers.HUnit       (testCase)
import           Test.Framework.Providers.QuickCheck2 (testProperty)
import           Test.Framework.TH                    (testGroupGenerator)
import           Test.HUnit                           (Assertion, assertEqual,
                                                      assertFailure, assertBool)
import qualified Test.QuickCheck                      as QuickCheck

instance QuickCheck.Arbitrary Form where
  arbitrary = QuickCheck.sized arbitraryForm

right :: Either a b -> b
right (Left _) = error "Expected Right"
right (Right x) = x

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

arbitraryBinop :: QuickCheck.Gen Form
arbitraryBinop = do
  str <- QuickCheck.elements [
    "*", "/", "%", "+", "<", ">", "<=", ">=", "==", "!=", "&&", "||", "<<",
    ">>", "|>", "<|", "=", "+=", "*=", "/=", "|=", "&=", "|"]
  x <- form
  y <- form
  return $ Binop str x y
  where form = QuickCheck.frequency [
            (90, arbitraryAtom),
            (10, QuickCheck.arbitrary)
          ]

arbitraryForm :: Int -> QuickCheck.Gen Form
arbitraryForm 0 = arbitraryAtom
arbitraryForm n = QuickCheck.frequency [
    (15, arbitraryAtom),
    (10, listOfSize 1),
    (5, listOfSize 2),
    (5, arbitraryBinop),
    (1, listOfSize 3),
    (1, listOfSize 4),
    (1, listOfSize 5),
    (1, listOfSize 0)
  ]
  where nextRandom = arbitraryForm (n `div` 2)
        listOfSize size = liftM Sexp $ QuickCheck.vectorOf size nextRandom

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
                        return ([c1] ++ showForms forms ++ [c2])

prop_validSyntax :: ValidSyntax -> Bool
prop_validSyntax (BalancedString s) = readWithOutcome True s
prop_validSyntax (ArbitraryIdent s) = readAndCheck s (== [Ident s])
prop_validSyntax (ArbitraryVector s) = readAndCheck s isVector
  where isVector [Vector _] = True
        isVector _          = False
prop_validSyntax (ArbitraryDict s) = readAndCheck s isMap
  where isMap [Map _] = True
        isMap _       = False


readOne :: String -> Form -> Assertion
readOne input expected = case readForms "" input of
  Left err     -> assertFailure $ "Error " ++ show err ++ " for input " ++ input
  Right []     -> assertFailure $ "No forms read for input " ++ input
  Right [form] -> assertEqual "Forms were not equal" expected form
  Right _      -> assertFailure $ "Multiple forms returned for input " ++ input

case_vector :: Assertion
case_vector = do
  readOne "[]" $ Vector []
  readOne "[1]" $ Vector [Int 1]
  readOne "[1 2]" $ Vector [Int 1, Int 2]

case_dict :: Assertion
case_dict = do
  readOne "{}" $ Map []
  readOne "{1}" $ Map [Int 1]
  readOne "{1 2}" $ Map [Int 1, Int 2]

readSame :: String -> String -> Assertion
x `readSame` y = assertEqual "Forms not equal!" (rightRead x) (rightRead y)
  where rightRead = right . readForms ""

assertParseError :: String -> Assertion
assertParseError input = assertBool "Parse expected to fail" parse
  where parse = case readForms "" input of
          (Left _)  -> True
          (Right _) -> False

case_indent :: Assertion
case_indent = do
  "alpha\n  bravo" `readSame`
      "(alpha bravo)"
  "alpha bravo\n  charlie" `readSame`
      "((alpha bravo) charlie)"
  "alpha bravo charlie\n  delta" `readSame`
      "((alpha bravo charlie) delta)"
  "alpha\n  bravo\n  charlie" `readSame`
      "(alpha bravo charlie)"
  "alpha\n  bravo charlie" `readSame`
      "(alpha (bravo charlie))"
  "alpha\n  12\n  14" `readSame`
      "(alpha 12 14)"
  "alpha\n  bravo\n    charlie" `readSame`
      "(alpha (bravo charlie))"
  "alpha\n  bravo charlie\n    delta" `readSame`
      "(alpha ((bravo charlie) delta))"
  "alpha\n  bravo\n    charlie delta" `readSame`
      "(alpha (bravo (charlie delta)))"
  "alpha\n  bravo charlie\n    delta echo" `readSame`
      "(alpha ((bravo charlie) (delta echo)))"
  "alpha bravo\n  charlie delta\n    echo foxtrot" `readSame`
      "((alpha bravo) ((charlie delta) (echo foxtrot)))"
  "alpha\n  ()" `readSame`
      "(alpha ())"
  "alpha\n  (bravo)" `readSame`
      "(alpha (bravo))"
  "alpha\n  (\nbravo)" `readSame`
      "(alpha (bravo))"
  "alpha\n  (bravo\n)" `readSame`
      "(alpha (bravo))"
  "alpha\n  {\nbravo}" `readSame`
      "(alpha {bravo})"
  "alpha\n  (bravo\n)\n  charlie" `readSame`
      "(alpha (bravo) charlie)"
  "alpha\n  (bravo\n  )\n  charlie" `readSame`
      "(alpha (bravo) charlie)"
  "alpha\n  (bravo\n    )\n  charlie" `readSame`
      "(alpha (bravo) charlie)"

case_indent_error :: Assertion
case_indent_error = assertParseError "alpha\n    bravo\n  charlie"

case_operator_error :: Assertion
case_operator_error = do
  assertParseError "foo .bar"
  assertParseError "foo. bar"

case_parens :: Assertion
case_parens = do
  readOne "(foo)" $ Sexp [Ident "foo"]
  readOne "(foo bar)" $ Sexp [Ident "foo",Ident "bar"]
  readOne "((foo))" $ Sexp [Sexp [Ident "foo"]]
  readOne "((foo bar))" $ Sexp [Sexp [Ident "foo",Ident "bar"]]
  readOne "(foo + bar)" $ Binop "+" (Ident "foo") (Ident "bar")
  readOne "((foo + bar))" $ Sexp [Binop "+" (Ident "foo") (Ident "bar")]

case_precedence :: Assertion
case_precedence = do  
  readOne "1 + 1 * 2" $
      Binop "+" (Int 1) (Binop "*" (Int 1) (Int 2))
  readOne "1 * 1 + 2" $
      Binop "+" (Binop "*" (Int 1) (Int 1)) (Int 2)
  readOne "1 * 1 * 2" $
      Binop "*" (Binop "*" (Int 1) (Int 1)) (Int 2)
  readOne "print 1 + print 2" $
      Binop "+" (Sexp [Ident "print",Int 1]) (Sexp [Ident "print",Int 2])
  readOne "foo.bar" $ Binop "." (Ident "foo") (Ident "bar")
  readOne "foo.bar.baz + 2" $
      Binop "+"
          (Binop "." (Binop "." (Ident "foo") (Ident "bar")) (Ident "baz"))
          (Int 2)
  readOne "print foo.bar.baz" $
      Sexp [Ident "print",
            Binop "." (Binop "." (Ident "foo") (Ident "bar")) (Ident "baz")]

readerTests :: Test
readerTests = $testGroupGenerator
