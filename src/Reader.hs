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
import           Text.Parsec.Expr   (Assoc(..), Operator(..),
                                    buildExpressionParser)
import           Text.Parsec.Indent (checkIndent, runIndent, withBlock)
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
showForm (Map s) = "{" ++ unwords (map showForm s) ++ "}"
showForm (Vector s) = "[" ++ unwords (map showForm s) ++ "]"
showForm (Binop s _ left right) = "(" ++ (showForm left) ++ " " ++ s ++ " " ++
                                  (showForm right) ++ ")"

-- | Uses the specified parser to parse a string, returning the result or a
-- ParseError.
runParse :: InputName -> CantorParser a -> String -> Either ParseError a
runParse source aParser input = runIndent source run
  where run = runParserT aParser NoIgnoreNewlines source input

-- | Reads a form, handling indentation correctly as a way to create a Sexp.
indentedForm :: CantorParser Form
indentedForm = do
  checkIndent
  sexp <- withBlock makeSexp readLine indentedForm
  return sexp
  where makeSexp first []   = first
        makeSexp first rest = Sexp $ first : rest

-- | Reads all of the forms on a single line of input.
readLine :: CantorParser Form
readLine = do
  forms <- many1 expression2
  char '\n' <?> "newline"
  skipSpaces
  return $ makeSexp forms
  where makeSexp [form] = form
        makeSexp forms  = Sexp forms

-- | Parses a single Form from the input stream.
readForm :: CantorParser Form
readForm = identifier <|>
           intOrFloat <|>
           initialHyphen <|>
           stringLiteral <|>
           readSexp <|>
           readBalanced '[' ']' Vector "vector" <|>
           readBalanced '{' '}' Map "map"

dthurn (Exp forms) = Sexp forms
dthurn b@(Binop name False left right) = Binop name True left right
dthurn exp = Sexp [exp]

readBalanced :: Char -> Char -> ([Form] -> Form) -> String -> CantorParser Form
readBalanced left right fn name = do
  char left <?> name ++ " literal"
  newlineStrategy <- getState
  putState IgnoreNewlines -- Until the closing delimiter, ignore newlines
  skipSpaces
  forms <- many readForm
  char right
  putState newlineStrategy -- Restore previous newline strategy
  skipSpaces
  return $ fn forms

readSexp :: CantorParser Form
readSexp = do
  char '(' <?> "s-expression"
  newlineStrategy <- getState
  putState IgnoreNewlines -- Until the closing delimiter, ignore newlines
  skipSpaces
  exp <- option (Exp []) expression3
  char ')'
  putState newlineStrategy -- Restore previous newline strategy
  skipSpaces
  return $ dthurn exp

leftInfix :: String -> CantorOperator
leftInfix name = Infix (operator name) AssocLeft

operatorTable :: CantorOperatorTable
operatorTable = [
    [leftInfix "*", leftInfix "/", leftInfix "%"],
    [leftInfix "+"],
    [leftInfix "<", leftInfix ">", leftInfix "<=", leftInfix ">=",
        leftInfix "==", leftInfix "!="],
    [leftInfix "&&"],
    [leftInfix "||"],
    [leftInfix "<<", leftInfix ">>", leftInfix "|>", leftInfix "<|"],
    [leftInfix "=", leftInfix "+=", leftInfix "*=", leftInfix "/=",
        leftInfix "|=", leftInfix "&="],
    [leftInfix "|"]
  ]

expression :: CantorParser Form
expression = buildExpressionParser operatorTable readForm <?> "expression"

sexpy :: CantorParser Form
sexpy = do
  forms <- many1 readForm
  return $ sexpify forms
  where sexpify [form] = form
        sexpify forms  = Sexp forms

expression2 = buildExpressionParser operatorTable sexpy <?> "expression"

sexpy2 :: CantorParser Form
sexpy2 = do
  forms <- many1 readForm
  return $ sexpify forms
  where sexpify [form] = form
        sexpify forms  = Exp forms

expression3 = buildExpressionParser operatorTable sexpy2 <?> "expression"
