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
readForms :: String -> Either ReadError [Form]
readForms ""     = Right []
readForms input = runParse topLevel (input ++ "\n")
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
showForm (Dot left right) = showForm left ++ "." ++ showForm right
showForm (Binop s left right) = "(" ++ showForm left ++ " " ++ s ++ " " ++
                                showForm right ++ ")"
showForm (Subscript left right) = showForm left ++ "[" ++ showForm right ++ "]"

-- | Uses the specified parser to parse a string, returning the result or a
-- ParseError.
runParse :: CantorParser a -> String -> Either ParseError a
runParse aParser input = runIndent "" run
  where run = runParserT aParser (SkippedWhitespace " \t") "" input

-- | Reads a form, handling indentation correctly as a way to create a Sexp.
indentedForm :: CantorParser Form
indentedForm = do
  checkIndent
  withBlock makeSexp readLine indentedForm
  where makeSexp first []   = first
        makeSexp first rest = Sexp $ first : rest

-- | Reads all of the forms on a single line of input.
readLine :: CantorParser Form
readLine = do
  expr <- expression
  char '\n' <?> "newline"
  skipSpaces
  return expr

-- | Parses a single Form from the input stream.
readForm :: CantorParser Form
readForm = identifier        <|>
           intOrFloat        <|>
           initialHyphen     <|>
           stringLiteral     <|>
           readParenthesized <|>
           readVector        <|>
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
  parsed <- withSkippedWhitespace " \t\n" $
    do skipSpaces
       p <- parser
       char right
       return p
  skipSpaces
  return parsed

-- | Reads a vector literal.
readVector :: CantorParser Form
readVector = fmap Vector $
    readBetween '[' ']' "vector literal" $ many complexForm

-- | Reads a map literal.
readMap :: CantorParser Form
readMap = fmap Map $
    readBetween '{' '}' "map literal" $ many complexForm

-- | Reads a parenthesized expression. If a single form is present in such an
-- expression, it's treated as a no-arg function invokation. If multiple forms
-- are present, it's treated as a grouping expression.
readParenthesized :: CantorParser Form
readParenthesized = try singleForm <|> multipleForms
  where readParens    = readBetween '(' ')' "expression"
        singleForm    = fmap (Sexp . return) $ readParens complexForm
        multipleForms = readParens $ option (Sexp []) expression

-- | Creates an operator parser for an infix, left associative operator with
-- the specified name.
leftInfix :: String -> CantorOperator
leftInfix name = Infix (operator name) AssocLeft

-- | Table of cantor operators, ordered from highest to lowest precedence. Note
-- that function invokation has higher precedence than any of these operators.
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

-- | Parses an invokation of the subscript operator, e.g. foo[2]
subscript :: CantorParser Form
subscript = do
  char '['
  skipSpaces
  expr <- expression
  char ']'
  skipSpaces
  return expr

-- | Runs the provided parser and then looks for zero of more invokations of the
-- subscript operator.
withSubscriptOperators :: CantorParser Form -> CantorParser Form
withSubscriptOperators parser = do
  parsed <- withSkippedWhitespace "" parser
  subscripts <- many subscript
  return $ foldl1 Subscript (parsed : subscripts)

-- | Parses a method call, a dot followed by 1) an identifier or 2) a
-- parenthesized expression.
methodCall :: CantorParser Form
methodCall = do
  string "."
  withSkippedWhitespace " \t\n" skipSpaces
  i <- withSubscriptOperators mcall
  skipSpaces
  return i
  where mcall = identifier <|> readBetween '(' ')' "method call" expression

-- | Parses a complex form, a form followed by zero more subscript operators
-- or method calls.
complexForm :: CantorParser Form
complexForm = do
  expr <- readForm
  skipSpaces
  methodCalls <- many (methodCall)
  return $ foldl1 fold (expr : methodCalls)
  where fold accum ident@(Ident _)    = Dot accum ident
        fold accum sexp@(Sexp (x:xs)) = Sexp $ Dot accum x : xs
        fold accum sub@(Subscript x y) = Subscript (fold accum x) y
        fold _ form = error ("Unexpected token in complexForm " ++ show form)

complexForm' :: CantorParser [Form]
complexForm' = do
  expr <- readForm
  skipSpaces
  methodCalls <- many (methodCall)
  return $ expr : methodCalls

-- | Parser which reads one or more forms. If multiple forms are read, this is
-- treated as a function call.
functionCall :: CantorParser Form
functionCall = do
  forms <- many1 complexForm
  return $ sexpify forms
  where sexpify [form] = form
        sexpify forms  = Sexp forms

-- | An operator precedence parser which correctly handles the operators in
-- operatorTable as well as the convention that multiple forms separated by
-- whitespace are a function call.
expression :: CantorParser Form
expression = buildExpressionParser operatorTable functionCall <?> "expression"
