-- This is free and unencumbered software released into the public domain.

{-# LANGUAGE TemplateHaskell #-}

-- | The Reader module is responsible for defining the Form datatype and for
-- implementing parser functionality for transforming inputs such as strings
-- into valid parse trees consisting of Forms.
module Reader (
  Form(..),
  ReadError,
  readString,
  runTests
) where

import Test.QuickCheck.All (quickCheckAll)

-- | Forms are the components of the Cantor parse tree. A form can be 1) an
-- atom, such as an identifier or literal, 2) an operator application to other
-- forms, or 3) A Sexp (a list of forms).
data Form = Int Integer
          | Float Double
          | Str String
          | Binop String Form Form
          | Ident String
          | Sexp [Form]
            deriving Show

-- | Indicates a failure during parsing.
type ReadError = String

-- | Parses a string and returns a list of top-level Forms present in the
-- string or an appropriate ReadError if the input is invalid.
readString :: String -> Either ReadError [Form]
readString = undefined

-- | Runs quickCheck on all prop_ functions in this module
runTests :: IO Bool
runTests = $quickCheckAll