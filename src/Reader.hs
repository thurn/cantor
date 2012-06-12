-- This is free and unencumbered software released into the public domain.

{-# LANGUAGE TemplateHaskell #-}

-- | The Reader module is responsible for defining the Form datatype and for
-- implementing parser functionality for transforming inputs such as strings
-- into valid parse trees consisting of Forms.
module Reader (
  Form(..),
  ReadError,
  readString,
  showForm,
  runTests
) where

import qualified Control.Monad
import qualified Control.Monad.State
import Text.Parsec
import qualified Text.Parsec.Indent as Parsec.Indent
import qualified Test.QuickCheck as QuickCheck
import Test.QuickCheck.All (quickCheckAll)

-- | Forms are the components of the Cantor parse tree. A form can be 1) an
-- atom, such as an identifier or literal, 2) an operator application to other
-- forms, or 3) A Sexp (a list of forms).
data Form = Int Integer
          | Float Double
          -- | Str String
          -- | Binop String Form Form
          | Ident String
          -- | Sexp [Form]
            deriving Show

-- | Indicates a failure during parsing.
type ReadError = String

-- | Parses a string and returns a list of top-level Forms present in the
-- string or an appropriate ReadError if the input is invalid.
readString :: String -> Either ReadError [Form]
readString = undefined

-- | Converts a form into a String representation of it. The resulting string
-- will consist of legal Cantor syntax and will be able to be parsed by
-- readString.
showForm :: Form -> String
showForm = show

-- | Runs quickCheck on all prop_ functions in this module
runTests :: IO Bool
runTests = $quickCheckAll

instance QuickCheck.Arbitrary Form where
  arbitrary = QuickCheck.oneof [
    Control.Monad.liftM Int QuickCheck.arbitrary,
    Control.Monad.liftM Float QuickCheck.arbitrary,
    Control.Monad.liftM Ident QuickCheck.arbitrary
    ]

-- | The type for Cantor parsers (indentation-sensitive)
type IParser a = ParsecT String () (Control.Monad.State.State SourcePos) a

-- | Uses the specified parser to parse a string, returning the result or a
-- ParseError.
runParse :: IParser a -> String -> Either ParseError a
runParse aParser input = Parsec.Indent.runIndent source runParser
  where source = "(unknown source)"
        runParser = runParserT aParser () source input

-- | Parses a single Form from the input stream.
readForm :: IParser Form
readForm = identifier <|>
           intOrFloat

-- | Parser which skips zero or more space or tab characters.
skipSpaces :: IParser String
skipSpaces = many (oneOf " \t")

-- | Symbols that may legally occur as an identifier as long as they are not the
-- first character.
identSymbols :: String
identSymbols = "!%&*-+=\\|?/<>"

-- | Parses an identifier
identifier :: IParser Form
identifier = do
  c <- letter
  cs <- many (alphaNum <|> oneOf identSymbols)
  skipSpaces
  return $ Ident $ c:cs

-- | Parses the fractional part of a float and returns a Form by appending the
-- provided string to the result and converting to a Double.
fraction :: String -> IParser Form
fraction s = do
  char '.'
  digits <- many1 digit
  return $ Float $ read $ s ++ "." ++ digits

-- | Parses either an Int or a Float, depending on whether a '.' character is
-- found in the stream.
intOrFloat :: IParser Form
intOrFloat = do
  int <- many1 digit
  form <- option (Int (read int)) (fraction int)
  skipSpaces
  return form
