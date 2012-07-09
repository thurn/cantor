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
import           Control.Applicative ((<$>))
import           Data.List           (intercalate)
import           Datatypes
import           Text.Parsec
import           Text.Parsec.Expr    (Assoc(..), Operator(..),
                                      buildExpressionParser)
import           Text.Parsec.Indent  (checkIndent, runIndent, withBlock, withPos)
import           Token

-- | Parses a string and returns a list of top-level Forms present in the
-- string or an appropriate ReadError if the input is invalid. The inputName
-- should be a succient description of the origin of this string, e.g., a
-- filename.
readForms :: String -> Either ReadError [Form]
readForms ""     = Right []
readForms input = runParse topLevel (input ++ "\n")
  where topLevel = do skipSpaces
                      forms <- withPos $ many indentedForm
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
  where run = runParserT aParser (SkippedWhitespace " ") "" input

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

-- | Parses a string literal
stringLiteral :: CantorParser Form
stringLiteral = do
  column <- sourceColumn <$> getPosition
  char '"'  <?> "string literal"
  forms <- many (stringUnquote <|> stringComponent <|> stringNewline column)
  char '"'
  skipSpaces
  return $ simplify forms
  where simplify []          = Str ""
        simplify [s@(Str _)] = s
        simplify xs          = Sexp $ Ident "str" : xs

stringNewline :: Column -> CantorParser Form
stringNewline column = do
  char '\n' <?> "line continuation"
  string (replicate column ' ') <?> "string literal indentation"
  return $ Str "\n"

-- | Parses a string unquote, a ~ followed by any complex form not containing
-- whitespace.
stringUnquote :: CantorParser Form
stringUnquote = do
  char '~' <?> "string unquote"
  form <- withSkippedWhitespace "" (complexFormSkipping "")
  return form

-- | Makes a parser which looks for an opening delimiter, then applies the
-- supplied parser, then looks for a closing delimiter.
readBetween :: Char -> -- | ^ Opening delimiter
               Char -> -- | ^ Closing delimiter
               String -> -- | ^ Name of what's being parsed
               CantorParser a -> -- | ^ Parser to apply between the delimiters
               CantorParser a
readBetween left right name parser = do
  char left <?> name
  parsed <- withSkippedWhitespace " \n" $
    do skipSpaces
       p <- parser
       char right
       return p
  skipSpaces
  return parsed

-- | Reads a vector literal.
readVector :: CantorParser Form
readVector = Vector <$>
    readBetween '[' ']' "vector literal" (many complexForm)

-- | Reads a map literal.
readMap :: CantorParser Form
readMap = Map <$>
    readBetween '{' '}' "map literal" (many complexForm)

-- | Reads a parenthesized expression. If a single form is present in such an
-- expression, it's treated as a no-arg function invokation. If multiple forms
-- are present, it's treated as a grouping expression.
readParenthesized :: CantorParser Form
readParenthesized = try singleForm <|> multipleForms
  where readParens    = readBetween '(' ')' "expression"
        singleForm    = (Sexp . return) <$> readParens complexForm
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
  return expr

-- | Runs the provided parser and then looks for zero of more invokations of the
-- subscript operator.
withSubscriptOperators :: CantorParser Form -> CantorParser Form
withSubscriptOperators parser = do
  parsed <- withSkippedWhitespace "" parser
  subscripts <- many subscript
  skipSpaces
  return $ foldl1 Subscript (parsed : subscripts)

-- | Parses a method call, a dot followed by 1) an identifier or 2) a
-- parenthesized expression.
methodCall :: String -> CantorParser Form
methodCall s = do
  string "."
  withSkippedWhitespace s skipSpaces
  i <- withSubscriptOperators mCall
  skipSpaces
  return i
  where mCall = identifier <|> readBetween '(' ')' "method call" expression

-- | Parses a complex form, a form followed by zero more subscript operators
-- or method calls. Uses the provided string as SkippedWhitespace following the
-- '.' operator in method calls.
complexFormSkipping :: String -> CantorParser Form
complexFormSkipping s = do
  expr <- withSubscriptOperators readForm
  skipSpaces
  methodCalls <- many (try (methodCall s))
  return $ foldl1 fold (expr : methodCalls)
  where fold accum ident@(Ident _)    = Dot accum ident
        fold accum (Sexp (x:xs)) = Sexp $ Dot accum x : xs
        fold accum (Subscript x y) = Subscript (fold accum x) y
        fold _ form = error ("Unexpected token in complexForm " ++ show form)

-- | Parses a complex form, skipping spaces and newlines after the '.' operator
-- for method calls.
complexForm :: CantorParser Form
complexForm = complexFormSkipping " \n"

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
