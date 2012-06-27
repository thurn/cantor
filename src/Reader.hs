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
showForm (Binop s left right) = "(" ++ (showForm left) ++ " " ++ s ++ " " ++
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
  forms <- many1 expression
  char '\n' <?> "newline"
  skipSpaces
  return $ makeSexp forms
  where makeSexp [form] = form
        makeSexp forms  = Sexp forms

-- | Parses a single Form from the input stream.
readForm :: CantorParser Form
readForm = identifier    <|>
           intOrFloat    <|>
           initialHyphen <|>
           stringLiteral <|>
           readParenthesized <|>
           readVector    <|>
           readMap

-- | Makes a parser which looks for an opening delimiter, then applies the
-- supplied parser, then looks for a closing delimiter.
readBetween :: Char -> -- | ^ Opening delimiter
               Char -> -- | ^ Closing delimiter
               String -> -- | ^ Name of what's being parsed
               CantorParser a -> -- | ^ Parser to apply between the delimiters
               CantorParser a
readBetween left right name parser = do
  char left <?> name
  newlineStrategy <- getState
  putState IgnoreNewlines -- Until the closing delimiter, ignore newlines
  skipSpaces
  parsed <- parser
  char right
  putState newlineStrategy -- Restore previous newline strategy
  skipSpaces
  return parsed

-- | Reads a vector literal
readVector :: CantorParser Form
readVector = fmap Vector $ readBetween '[' ']' "vector literal" $ many readForm

-- | Reads a map literal
readMap :: CantorParser Form
readMap = fmap Map $ readBetween '{' '}' "map literal" $ many readForm

-- | Reads a parenthesized expression. If a single form is present in such an
-- expression, it's treated as a no-arg function invokation. If multiple forms
-- are present, it's treated as a grouping expression
readParenthesized :: CantorParser Form
readParenthesized = try singleForm <|> multipleForms
  where readParens    = readBetween '(' ')' "expression"
        singleForm    = fmap (Sexp . return) $ readParens readForm
        multipleForms = readParens $ option (Sexp []) expression

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

makeSexp :: CantorParser Form
makeSexp = do
  forms <- many1 readForm
  return $ sexpify forms
  where sexpify [form] = form
        sexpify forms  = Sexp forms

expression :: CantorParser Form
expression = buildExpressionParser operatorTable makeSexp <?> "expression"
