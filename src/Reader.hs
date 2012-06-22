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

import           Data.List          (intercalate)
import           Datatypes
import           Text.Parsec
import           Text.Parsec.Indent (runIndent, withBlock, checkIndent)
import           Token

-- | Parses a string and returns a list of top-level Forms present in the
-- string or an appropriate ReadError if the input is invalid. The inputName
-- should be a succient description of the origin of this string, e.g., a
-- filename.
readForms :: InputName -> String -> Either ReadError [Form]
readForms _ ""             = Right []
readForms inputName input = runParse inputName topLevel (input ++ "\n")
  where topLevel = do
          forms <- many indentedForm
          eof
          return forms

-- | Converts a list of forms into a String representation. The resulting string
-- will consist of legal Cantor syntax and will be able to be parsed by
-- readForms.
showForms :: [Form] -> String
showForms forms = intercalate "\n" $ map showForm forms

showForm :: Form -> String
showForm (Int i)   = show i
showForm (Float f) = show f
showForm (Ident i) = i
showForm (Str s) = show s
showForm (Sexp s) = "(" ++ unwords (map showForm s) ++ ")"

-- | Uses the specified parser to parse a string, returning the result or a
-- ParseError.
runParse :: InputName -> CantorParser a -> String -> Either ParseError a
runParse source aParser input = runIndent source run
  where run = runParserT aParser NoIgnoreNewlines source input

indentedForm :: CantorParser Form
indentedForm = do
  checkIndent
  sexp <- withBlock makeSexp readLine indentedForm
  return sexp
  where makeSexp first []   = first
        makeSexp first rest = Sexp $ first : rest

readLine :: CantorParser Form
readLine = do
  forms <- many1 (readForm SignificantNewlines)
  char '\n'
  skipSpaces SignificantNewlines
  return $ makeSexp forms
  where makeSexp [form] = form
        makeSexp forms  = Sexp forms

-- | Parses a single Form from the input stream.
readForm :: SkipNewlines -> CantorParser Form
readForm skipNewlines = identifier skipNewlines <|>
                        intOrFloat skipNewlines <|>
                        initialHyphen skipNewlines <|>
                        stringLiteral skipNewlines<|>
                        readBalanced skipNewlines '(' ')' id <|>
                        readBalanced skipNewlines '[' ']' (Ident "vector" :) <|>
                        readBalanced skipNewlines '{' '}' (Ident "dict" :)

readBalanced :: SkipNewlines -> 
             Char ->
             Char ->
             ([Form] -> [Form]) ->
             CantorParser Form
readBalanced skipNewlines left right fn = do
  char left
  newlineStrategy <- getState
  putState IgnoreNewlines -- Until the closing delimiter, ignore newlines
  skipSpaces SkipNewlines
  sexp <- many (readForm SkipNewlines)
  char right
  putState newlineStrategy -- Restore previous newline strategy
  skipSpaces skipNewlines
  return $ Sexp $ fn sexp
