-- This is free and unencumbered software released into the public domain.

-- | The Reader module is responsible for defining the Form datatype and for
-- implementing parser functionality for transforming inputs such as strings
-- into valid parse trees consisting of Forms.
module Reader (
  Form(..),
  ReadError,
  InputName,
  readForms,
  showForms,
) where

import           Datatypes
import           Text.Parsec

import qualified Data.List          as List
import qualified Text.Parsec.Indent as Parsec.Indent
import qualified Token              as Token

-- | Parses a string and returns a list of top-level Forms present in the
-- string or an appropriate ReadError if the input is invalid. The inputName
-- should be a succient description of the origin of this string, e.g., a
-- filename.
readForms :: InputName -> String -> Either ReadError [Form]
readForms inputName = runParse inputName topLevel
  where topLevel = do
          forms <- many readForm
          eof
          return forms

-- | Converts a list of forms into a String representation. The resulting string
-- will consist of legal Cantor syntax and will be able to be parsed by
-- readForms.
showForms :: [Form] -> String
showForms forms = List.intercalate "\t" $ map showForm forms

showForm :: Form -> String
showForm (Int i)   = show i
showForm (Float f) = show f
showForm (Ident i) = i
showForm (Str s) = show s
showForm (Sexp s) = "(" ++ List.intercalate " " (map showForm s) ++ ")"

-- | Uses the specified parser to parse a string, returning the result or a
-- ParseError.
runParse :: InputName -> CantorParser a -> String -> Either ParseError a
runParse source aParser input = Parsec.Indent.runIndent source run
  where run = runParserT aParser () source input

-- | Parses a single Form from the input stream.
readForm :: CantorParser Form
readForm = Token.identifier <|>
           Token.intOrFloat <|>
           Token.initialHyphen <|>
           Token.stringLiteral <|>
           readSexp <|>
           readVector <|>
           readDict

readSexp :: CantorParser Form
readSexp = do
  sexp <- between (char '(') (char ')') (many readForm)
  Token.skipSpaces
  return $ Sexp sexp

readVector :: CantorParser Form
readVector = do
  sexp <- between (char '[') (char ']') (many readForm)
  Token.skipSpaces
  return $ Sexp $ (Ident "vector") : sexp

readDict :: CantorParser Form
readDict = do
  sexp <- between (char '{') (char '}') (many readForm)
  Token.skipSpaces
  return $ Sexp $ (Ident "dict") : sexp
