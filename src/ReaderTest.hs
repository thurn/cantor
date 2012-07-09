-- This is free and unencumbered software released into the public domain.

{-# LANGUAGE TemplateHaskell #-}

module ReaderTest (
  readerTests
) where

import           Control.Applicative                  ((<*>), (<$>))
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

arbitraryAtom :: Gen Form
arbitraryAtom = oneof [
  Int   <$> arbitrary,
  Float <$> arbitrary,
  Str   <$> arbitraryString,
  Ident <$> arbitraryIdent
  ]

identInitialChars :: String
identInitialChars = ['a' .. 'z'] ++ ['A' .. 'Z']

identSubsequentChars :: String
identSubsequentChars = identInitialChars ++ "0123456789!%&*-+=|?/<>"

-- | Generate a string that's a valid identifier name.
arbitraryIdent :: Gen String
arbitraryIdent = do
  c  <- elements identInitialChars
  cs <- listOf $ elements identSubsequentChars
  return $ c:cs

arbitraryString :: Gen String
arbitraryString = suchThat arbitrary (all (/= '~'))

newtype ArbitraryString = ArbitraryString String deriving (Show, Eq)

instance Arbitrary ArbitraryString where
  arbitrary = ArbitraryString <$> arbitraryString

arbitraryBinop :: Gen Form -> Gen Form -> Gen Form
arbitraryBinop genForm1 genForm2 = Binop <$> str <*> genForm1 <*> genForm2
  where str = elements  ["*", "/", "%", "+", "<", ">", "<=", ">=", "==", "!=",
                         "&&", "||", "<<", ">>", "|>", "<|", "=", "+=", "*=",
                         "/=", "|=", "&=", "|"]

arbitraryDot :: Gen Form -> Gen Form
arbitraryDot genForm = Dot <$> genForm <*> (Ident <$> arbitraryIdent)

arbitrarySubscript :: Gen Form -> Gen Form -> Gen Form
arbitrarySubscript genForm1 genForm2 = Subscript <$> genForm1 <*> genForm2

arbitraryForm :: Int -> Gen Form
arbitraryForm 0 = arbitraryAtom
arbitraryForm n = frequency [
    (30, arbitraryAtom),
    (10, listOfSize 1),
    (5, listOfSize 2),
    (5, arbitraryBinop nextRandom nextRandom),
    (5, arbitrarySubscript nextRandom nextRandom),
    (5, arbitraryDot nextRandom),
    (1, listOfSize 3),
    (1, listOfSize 4),
    (1, listOfSize 5),
    (1, listOfSize 0)
  ]
  where nextRandom = arbitraryForm (n `div` 2)
        listOfSize size = do
          fn <- elements [Map, Vector, Sexp]
          fn <$> vectorOf size nextRandom

prop_stringConcat :: ArbitraryString -> ArbitraryString -> Bool
prop_stringConcat (ArbitraryString s1) (ArbitraryString s2) =
    readAndCheck (show strings) (== [Str strings])
  where strings   = s1 ++ s2

showDropDelimiters :: String -> String
showDropDelimiters = tail . init . show

prop_stringUnquote :: ArbitraryString -> Form -> ArbitraryString -> Bool
prop_stringUnquote (ArbitraryString s1) form (ArbitraryString s2) =
    readAndCheck generated $ (== [expected]) . (map dropEmptyStrs)
  where generated = "\"" ++ showDropDelimiters s1 ++ "~" ++ showForms [form] ++
                    "\\&" ++ showDropDelimiters s2 ++ "\""
        expected = dropEmptyStrs $ Sexp [Ident "str", Str s1, form, Str s2]
        dropEmptyStrs (Sexp xs) = Sexp $ filter (/= Str "") xs
        dropEmptyStrs x         = x

-- | Reads forms from the provided string and applies the provided predicate to
-- the result.
readAndCheck :: String -> ([Form] -> Bool) -> Bool
readAndCheck input predicate = case readForms input of
  Left _      -> False
  Right forms -> predicate forms

-- | Reads forms from the input and checks whether the parse succeeded,
-- returning (parseSucceeded XNOR shouldSucceed)
readWithOutcome :: Bool -> String -> Bool
readWithOutcome shouldSucceed input = case readForms input of
  Left _  -> not shouldSucceed
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

-- | Represents strings which are *not* valid Cantor syntax
data InvalidSyntax = UnbalancedString String
                     deriving (Show, Read, Eq)

instance Arbitrary InvalidSyntax where
  arbitrary = oneof [
    UnbalancedString <$> balancedString not
    ]

-- | Checks if a string consists of balanced parentheses
isBalanced :: String -> Bool
isBalanced list = null $ foldl' op [] list
  where op ('(':xs) ')' = xs
        op ('{':xs) '}' = xs
        op xs x         = x:xs

-- | Generates strings consisting of either balanced parentheses (by passing
-- 'id' as the predicate) or unbalanced parentheses (by passing 'not')
balancedString :: (Bool -> Bool) -> Gen String
balancedString p = suchThat parens (p . isBalanced)
  where parens = listOf $ elements "(){}"

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
    BalancedString  <$> balancedString id,
    ArbitraryIdent  <$> arbitraryIdent,
    ArbitraryVector <$> arbitraryBetween '[' ']',
    ArbitraryDict   <$> arbitraryBetween '{' '}'
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

(--->) :: String -> Form -> Assertion
input ---> expected = case readForms input of
  Left err     -> assertFailure $ "Error " ++ show err ++ " for input " ++ input
  Right []     -> assertFailure $ "No forms read for input " ++ input
  Right [form] -> assertEqual "Forms were not equal" expected form
  Right _      -> assertFailure $ "Multiple forms returned for input " ++ input

case_vector :: Assertion
case_vector = do
  "[]" ---> Vector []
  "[1]" ---> Vector [Int 1]
  "[1 2]" ---> Vector [Int 1, Int 2]

case_dict :: Assertion
case_dict = do
  "{}" ---> Map []
  "{1}" ---> Map [Int 1]
  "{1 2}" ---> Map [Int 1, Int 2]

readSame :: String -> String -> Assertion
x `readSame` y = assertEqual "Forms not equal!" (rightRead x) (rightRead y)
  where rightRead s = case readForms s of
          (Left err)    ->
              error ("\n\n" ++ show err ++ "\n\nFor input:\n\n" ++ s ++ "\n")
          (Right forms) -> forms

assertParseError :: String -> Assertion
assertParseError input = assertBool "Parse expected to fail" parse
  where parse = case readForms input of
          (Left _)  -> True
          (Right _) -> False

case_parseError :: Assertion
case_parseError = mapM_ assertParseError [
  "alpha\n    bravo\n  charlie",
  "foo[]",
  "foo.()",
  "foo\tbar",
  "\"foo\nbar\"",
  "\"foo\n~bar\"",
  "\"foo\n\"",
  "\"\nfoo\"",
  "\"\n\""]

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
  " alpha bravo" `readSame` "(alpha bravo)"
  " alpha bravo\n   charlie" `readSame` "((alpha bravo) charlie)"

case_parens :: Assertion
case_parens = do
  "(foo)" ---> Sexp [Ident "foo"]
  "(foo bar)" ---> Sexp [Ident "foo",Ident "bar"]
  "((foo))" ---> Sexp [Sexp [Ident "foo"]]
  "((foo bar))" ---> Sexp [Sexp [Ident "foo",Ident "bar"]]
  "(foo + bar)" ---> Binop "+" (Ident "foo") (Ident "bar")
  "((foo + bar))" ---> Sexp [Binop "+" (Ident "foo") (Ident "bar")]

case_operators :: Assertion
case_operators = do
  "1 + 1 * 2" --->
      Binop "+" (Int 1) (Binop "*" (Int 1) (Int 2))
  "1 * 1 + 2" --->
      Binop "+" (Binop "*" (Int 1) (Int 1)) (Int 2)
  "1 * 1 * 2" --->
      Binop "*" (Binop "*" (Int 1) (Int 1)) (Int 2)
  "print 1 + print 2" --->
      Binop "+" (Sexp [Ident "print",Int 1]) (Sexp [Ident "print",Int 2])
  "foo.bar" ---> Dot (Ident "foo") (Ident "bar")
  "foo. bar" ---> Dot (Ident "foo") (Ident "bar")
  "foo .bar" ---> Dot (Ident "foo") (Ident "bar")
  "foo.bar.baz + 2" --->
      Binop "+"
          (Dot (Dot (Ident "foo") (Ident "bar")) (Ident "baz"))
          (Int 2)
  "print foo.bar.baz" --->
      Sexp [Ident "print",
            Dot (Dot (Ident "foo") (Ident "bar")) (Ident "baz")]
  "1 +\n1" ---> Binop "+" (Int 1) (Int 1)
  "foo .\nbar" ---> Dot (Ident "foo") (Ident "bar")
  "foo.\nbar.\nbaz" ---> Dot (Dot (Ident "foo") (Ident "bar")) (Ident "baz")
  "foo.bar 1 2" ---> Sexp [Dot (Ident "foo") (Ident "bar"),Int 1,Int 2]
  "bar foo.baz 1 2" --->
      Sexp [Ident "bar",Dot (Ident "foo") (Ident "baz"),Int 1,Int 2]
  "list.filter foo.map baz.sort" --->
      Sexp [Dot (Ident "list") (Ident "filter"),
            Dot (Ident "foo") (Ident "map"),
            Dot (Ident "baz") (Ident "sort")]
  "(list.(filter foo).(map baz).sort)" --->
      Sexp [Dot (Sexp [Dot (Sexp [Dot (Ident "list")
                                      (Ident "filter"),
                                  Ident "foo"])
                           (Ident "map"),
                       Ident "baz"])
                (Ident "sort")]
  "list.(filter foo).(map baz).(sort)" --->
      Dot (Sexp [Dot (Sexp [Dot (Ident "list")
                                (Ident "filter"),
                            Ident "foo"])
                     (Ident "map"),
                Ident "baz"])
          (Ident "sort")

case_subscript :: Assertion
case_subscript = do
  "foo[2]" ---> Subscript (Ident "foo") (Int 2)
  "foo[2] + bar[2]" --->
      Binop "+" (Subscript (Ident "foo")
                           (Int 2))
                (Subscript (Ident "bar")
                           (Int 2))
  "(foo + bar)[2]" --->
    Subscript (Binop "+" (Ident "foo")
                         (Ident "bar"))
              (Int 2)
  "foo.bar[1]" --->
      Subscript (Dot (Ident "foo")
                     (Ident "bar"))
                (Int 1)
  "foo.bar[1].baz" --->
      Dot (Subscript (Dot (Ident "foo")
                          (Ident "bar"))
                     (Int 1))
          (Ident "baz")
  "foo.bar[1].baz[2]" --->
      Subscript (Dot (Subscript (Dot (Ident "foo")
                                     (Ident "bar"))
                                (Int 1))
                     (Ident "baz"))
                (Int 2)
  "foo.bar[1][2].baz" --->
      Dot (Subscript (Subscript (Dot (Ident "foo")
                                     (Ident "bar"))
                                (Int 1))
                     (Int 2))
          (Ident "baz")
  "foo.bar.baz[1]" --->
      Subscript (Dot (Dot (Ident "foo")
                          (Ident "bar"))
                     (Ident "baz"))
                (Int 1)
  "foo.(bar baz)[1]" --->
      Subscript (Sexp [Dot (Ident "foo")
                           (Ident "bar"),
                      Ident "baz"])
                (Int 1)
  "foo.(bar baz)[1].(a b)" --->
      Sexp [Dot (Subscript (Sexp [Dot (Ident "foo")
                                      (Ident "bar"),
                                 Ident "baz"])
                           (Int 1))
                (Ident "a"),
           Ident "b"]
  "[foo.bar[2].baz]" --->
      Vector [Dot (Subscript (Dot (Ident "foo")
                                  (Ident "bar"))
                             (Int 2))
                  (Ident "baz")]
  "[1][2][3]" --->
      Subscript (Subscript (Vector [Int 1])
                           (Int 2))
                (Int 3)
  "foo[bar] [baz]" --->
      Sexp [Subscript (Ident "foo")
                      (Ident "bar"),
            Vector [Ident "baz"]]

case_string :: Assertion
case_string = do
  "\"~foo\"" ---> Sexp [Ident "str",Ident "foo"]
  "\"~1\"" ---> Sexp [Ident "str",Int 1]
  "\"~foo bar\"" ---> Sexp [Ident "str",Ident "foo",Str " bar"]
  "\"foo ~bar baz ~qux ~alpha\"" --->
      Sexp [Ident "str",Str "foo ",Ident "bar",Str " baz ",Ident "qux",Str " ",
            Ident "alpha"]
  "\"~\"str\"\"" ---> Str "str"
  "\"~(foo bar baz)\"" --->
      Sexp [Ident "str",Sexp [Ident "foo",Ident "bar",Ident "baz"]]
  "\"~(1 + 2)\"" --->
      Sexp [Ident "str",Binop "+" (Int 1) (Int 2)]
  "\"~[1 2 3]\"" ---> Sexp [Ident "str",Vector [Int 1,Int 2,Int 3]]
  "\"~foo.bar\"" ---> Sexp [Ident "str",Dot (Ident "foo") (Ident "bar")]
  "\"~(foo.bar)\"" --->
      Sexp [Ident "str",Sexp [Dot (Ident "foo") (Ident "bar")]]
  "\"~foo.bar .baz\"" --->
      Sexp [Ident "str",Dot (Ident "foo") (Ident "bar"),Str " .baz"]
  "\"~foo.bar[2]\"" --->
      Sexp [Ident "str",Subscript (Dot (Ident "foo") (Ident "bar")) (Int 2)]
  "\"~foo.bar.\"" --->
      Sexp [Ident "str",Dot (Ident "foo") (Ident "bar"),Str "."]
  "\"~foo.bar [2]\"" --->
      Sexp [Ident "str",Dot (Ident "foo") (Ident "bar"),Str " [2]"]
  "\"~foo.bar.baz[2].qux\"" --->
      Sexp [Ident "str",Dot (Subscript (Dot (Dot (Ident "foo")
                                                 (Ident "bar"))
                                            (Ident "baz"))
                                       (Int 2))
                            (Ident "qux")]
  "\"\"" ---> Str ""
  "\"foo\n bar\"" ---> Sexp [Ident "str",Str "foo",Str "\n",Str "bar"]
  "\"foo\n ~bar\"" ---> Sexp [Ident "str",Str "foo",Str "\n",Ident "bar"]
  "\"foo\n \"" ---> Sexp [Ident "str",Str "foo",Str "\n"]
  "\"\n foo\"" ---> Sexp [Ident "str",Str "\n", Str "foo"]
  "\"\n \"" ---> Str "\n"
  

readerTests :: Test
readerTests = $testGroupGenerator