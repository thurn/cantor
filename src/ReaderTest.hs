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
import           Test.QuickCheck                      (Arbitrary, Gen, sized,
    oneof, arbitrary, frequency, listOf, elements, vectorOf, suchThat)

instance Arbitrary Form where
  arbitrary = sized arbitraryForm

right :: Either a b -> b
right (Left _) = error "Expected Right"
right (Right x) = x

arbitraryAtom :: Gen Form
arbitraryAtom = oneof [
  liftM Int arbitrary,
  liftM Float arbitrary,
  liftM Str arbitraryString,
  liftM Ident arbitraryIdent
  ]

identInitialChars :: String
identInitialChars = ['a' .. 'z'] ++ ['A' .. 'Z']

identSubsequentChars :: String
identSubsequentChars = identInitialChars ++ "0123456789!%&*-+=\\|?/<>"

-- | Generate a string that's a valid identifier name.
arbitraryIdent :: Gen String
arbitraryIdent = do
  c  <- elements identInitialChars
  cs <- listOf $ elements identSubsequentChars
  return $ c:cs

arbitraryString :: Gen String
arbitraryString = do
  s <- arbitrary
  return $ show (s :: String)

arbitraryBinop :: Gen Form
arbitraryBinop = do
  str <- elements [
    "*", "/", "%", "+", "<", ">", "<=", ">=", "==", "!=", "&&", "||", "<<",
    ">>", "|>", "<|", "=", "+=", "*=", "/=", "|=", "&=", "|"]
  x <- frequentlyAtom
  y <- frequentlyAtom
  return $ Binop str x y

frequentlyAtom :: Gen Form
frequentlyAtom = frequency [
                   (90, arbitraryAtom),
                   (10, arbitrary)
                 ]
  
arbitraryForm :: Int -> Gen Form
arbitraryForm 0 = arbitraryAtom
arbitraryForm n = frequency [
    (15, arbitraryAtom),
    (10, listOfSize 1),
    (5, listOfSize 2),
    (5, arbitraryBinop),
    (5, arbitraryDot),
    (1, listOfSize 3),
    (1, listOfSize 4),
    (1, listOfSize 5),
    (1, listOfSize 0)
  ]
  where nextRandom = arbitraryForm (n `div` 2)
        listOfSize size = do
          fn <- elements [Map, Vector, Sexp]
          liftM fn $ vectorOf size nextRandom
        arbitraryDot = do
          x <- frequentlyAtom
          y <- liftM Ident arbitraryIdent
          return $ Dot x y

-- | Reads forms from the provided string and applies the provided predicate to
-- the result.
readAndCheck :: String -> ([Form] -> Bool) -> Bool
readAndCheck input predicate = case readForms input of
  Left _ -> False
  Right forms -> predicate forms

-- | Reads forms from the input and checks whether the parse succeeded,
-- returning (parseSucceeded XNOR shouldSucceed)
readWithOutcome :: Bool -> String -> Bool
readWithOutcome shouldSucceed input = case readForms input of
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

instance Arbitrary InvalidSyntax where
  arbitrary = oneof [
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
balancedString :: (Bool -> Bool) -> Gen String
balancedString p = suchThat parens (p . isBalanced)
  where parens = listOf $ elements "()[]{}"

prop_invalidSyntax :: InvalidSyntax -> Bool
prop_invalidSyntax (UnbalancedString s) = readWithOutcome False s

-- | Represents various strings which should parse as valid Cantor syntax
data ValidSyntax = BalancedString String
                 | ArbitraryIdent String
                 | ArbitraryVector String
                 | ArbitraryDict String
                   deriving (Show, Read, Eq)

instance Arbitrary ValidSyntax where
  arbitrary = oneof [
    liftM BalancedString (balancedString id),
    liftM ArbitraryIdent arbitraryIdent,
    liftM ArbitraryVector (arbitraryBetween '[' ']'),
    liftM ArbitraryDict (arbitraryBetween '{' '}')
    ]

arbitraryBetween :: Char -> Char -> Gen String
arbitraryBetween c1 c2 = oneof $ map generate [0 .. 4]
  where generate n = do forms <- vectorOf n arbitrary
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
readOne input expected = case readForms input of
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
  where rightRead = right . readForms

assertParseError :: String -> Assertion
assertParseError input = assertBool "Parse expected to fail" parse
  where parse = case readForms input of
          (Left _)  -> True
          (Right _) -> False

case_indent :: Assertion
case_indent = do
  "alpha bravo" `readSame` "(alpha bravo)"
  "[alpha] bravo" `readSame` "([alpha] bravo)"
  "alpha [bravo]" `readSame` "(alpha [bravo])"
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

case_parens :: Assertion
case_parens = do
  readOne "(foo)" $ Sexp [Ident "foo"]
  readOne "(foo bar)" $ Sexp [Ident "foo",Ident "bar"]
  readOne "((foo))" $ Sexp [Sexp [Ident "foo"]]
  readOne "((foo bar))" $ Sexp [Sexp [Ident "foo",Ident "bar"]]
  readOne "(foo + bar)" $ Binop "+" (Ident "foo") (Ident "bar")
  readOne "((foo + bar))" $ Sexp [Binop "+" (Ident "foo") (Ident "bar")]

case_operators :: Assertion
case_operators = do  
  readOne "1 + 1 * 2" $
      Binop "+" (Int 1) (Binop "*" (Int 1) (Int 2))
  readOne "1 * 1 + 2" $
      Binop "+" (Binop "*" (Int 1) (Int 1)) (Int 2)
  readOne "1 * 1 * 2" $
      Binop "*" (Binop "*" (Int 1) (Int 1)) (Int 2)
  readOne "print 1 + print 2" $
      Binop "+" (Sexp [Ident "print",Int 1]) (Sexp [Ident "print",Int 2])
  readOne "foo.bar" $ Dot (Ident "foo") (Ident "bar")
  readOne "foo. bar" $ Dot (Ident "foo") (Ident "bar")
  readOne "foo .bar" $ Dot (Ident "foo") (Ident "bar")
  readOne "foo.bar.baz + 2" $
      Binop "+"
          (Dot (Dot (Ident "foo") (Ident "bar")) (Ident "baz"))
          (Int 2)
  readOne "print foo.bar.baz" $
      Sexp [Ident "print",
            Dot (Dot (Ident "foo") (Ident "bar")) (Ident "baz")]
  readOne "1 +\n1" $ Binop "+" (Int 1) (Int 1)
  readOne "foo .\nbar" $ Dot (Ident "foo") (Ident "bar")
  readOne "foo.\nbar.\nbaz" $ Dot (Dot (Ident "foo") (Ident "bar")) (Ident "baz")
  readOne "foo.bar 1 2" $ Sexp [Dot (Ident "foo") (Ident "bar"),Int 1,Int 2]  
  readOne "bar foo.baz 1 2" $
      Sexp [Ident "bar",Dot (Ident "foo") (Ident "baz"),Int 1,Int 2]
  readOne "list.filter foo.map baz.sort" $
      Sexp [Dot (Ident "list") (Ident "filter"),
            Dot (Ident "foo") (Ident "map"),
            Dot (Ident "baz") (Ident "sort")]
  readOne "(list.(filter foo).(map baz).sort)" $
      Sexp [Dot (Sexp [Dot (Sexp [Dot (Ident "list")
                                      (Ident "filter"),
                                  Ident "foo"])
                           (Ident "map"),
                       Ident "baz"])
                (Ident "sort")]
  readOne "list.(filter foo).(map baz).(sort)" $
      Dot (Sexp [Dot (Sexp [Dot (Ident "list")
                                (Ident "filter"),
                            Ident "foo"])
                     (Ident "map"),
                Ident "baz"])
          (Ident "sort")

case_subscript :: Assertion
case_subscript = do
  readOne "foo[2]" $ Subscript (Ident "foo") (Int 2)
  readOne "foo[2] + bar[2]" $
      Binop "+" (Subscript (Ident "foo")
                           (Int 2))
                (Subscript (Ident "bar")
                           (Int 2))
  readOne "(foo + bar)[2]" $
    Subscript (Binop "+" (Ident "foo")
                         (Ident "bar"))
              (Int 2)
  readOne "foo.bar[1]" $
      Subscript (Dot (Ident "foo")
                     (Ident "bar"))
                (Int 1)
  readOne "foo.bar[1].baz" $
      Dot (Subscript (Dot (Ident "foo")
                          (Ident "bar"))
                     (Int 1))
          (Ident "baz")
  readOne "foo.bar[1].baz[2]" $
      Subscript (Dot (Subscript (Dot (Ident "foo")
                                     (Ident "bar"))
                                (Int 1))
                     (Ident "baz"))
                (Int 2)
  readOne "foo.bar[1][2].baz" $
      Dot (Subscript (Subscript (Dot (Ident "foo")
                                     (Ident "bar"))
                                (Int 1))
                     (Int 2))
          (Ident "baz")
  readOne "foo.bar.baz[1]" $
      Subscript (Dot (Dot (Ident "foo")
                          (Ident "bar"))
                     (Ident "baz"))
                (Int 1)
  readOne "foo.(bar baz)[1]" $
      Subscript (Sexp [Dot (Ident "foo")
                           (Ident "bar"),
                      Ident "baz"])
                (Int 1)
  readOne "foo.(bar baz)[1].(a b)" $
      Sexp [Dot (Subscript (Sexp [Dot (Ident "foo")
                                      (Ident "bar"),
                                 Ident "baz"])
                           (Int 1))
                (Ident "a"),
           Ident "b"]
  readOne "[foo.bar[2].baz]" $
      Vector [Dot (Subscript (Dot (Ident "foo")
                                  (Ident "bar"))
                             (Int 2))
                  (Ident "baz")]
  readOne "[1][2][3]" $
      Subscript (Subscript (Vector [Int 1])
                           (Int 2))
                (Int 3)

readerTests :: Test
readerTests = $testGroupGenerator











