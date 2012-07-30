-- This is free and unencumbered software released into the public domain.

{-# LANGUAGE TemplateHaskell #-}

module TranslatorTest (
  translatorTests
) where

import Datatypes
import Test.Framework (Test)
import Test.Framework.Providers.HUnit (testCase)
import Test.Framework.TH (testGroupGenerator)
import Test.HUnit (Assertion, assertEqual, assertFailure, assertBool)
import Translator (translate)

(--->) :: Form -> String -> Assertion
input ---> expected = case translate input of
  Left err -> assertFailure $ "Error " ++ show err ++ " for input " ++ show input
  Right str -> assertEqual "Strings were not equal" expected str

assertTranslationError :: Form -> Assertion
assertTranslationError input = assertBool "Translation expected to fail" trans
  where trans = case translate input of
          (Left _) -> True
          (Right _) -> False

case_atoms :: Assertion
case_atoms = do
  Int 0 ---> "0"
  Int (-123) ---> "-123"
  Float 0.0 ---> "0.0"
  Float (-123.321) ---> "-123.321"
  Ident "foo" ---> "foo"
  Str "" ---> "\"\""
  Str "awesome" ---> "\"awesome\""

makeCpp = Exp (Ident "cpp")

case_cpp :: Assertion
case_cpp = do
  makeCpp [Str ""] ---> ""
  makeCpp [Str "foo"] ---> "foo"
  makeCpp [Exp (Ident "str") []] ---> ""
  makeCpp [Exp (Ident "str") [Int 1]] ---> "1"
  makeCpp [Exp (Ident "str") [Int 1, Str " "]] ---> "1 "
  makeCpp [Exp (Ident "str") [Int 1, Str " + ", Int 2]] ---> "1 + 2"
  assertTranslationError $ makeCpp []
  assertTranslationError $ makeCpp [Int 12]
  assertTranslationError $ makeCpp [Exp (Int 12) []]
  assertTranslationError $ makeCpp [Exp (Ident "bar") []]

translatorTests :: Test
translatorTests = $testGroupGenerator
