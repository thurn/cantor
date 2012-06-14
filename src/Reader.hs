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

import qualified Control.Monad.State
import qualified Data.List as List
import Text.Parsec
import qualified Text.Parsec.Indent as Parsec.Indent

-- | Forms are the components of the Cantor parse tree. A form can be 1) an
-- atom, such as an identifier or literal, 2) an operator application to other
-- forms, or 3) A Sexp (a list of forms).
data Form = Int Integer
          | Float Double
          -- | Str String
          -- | Binop String Form Form
          | Ident String
          -- | Sexp [Form]
            deriving (Show, Read, Eq)

-- | Indicates a failure during parsing.
type ReadError = ParseError
type InputName = String

-- | Parses a string and returns a list of top-level Forms present in the
-- string or an appropriate ReadError if the input is invalid. The inputName
-- should be a succient description of the origin of this string, e.g., a
-- filename.
readForms :: InputName -> String -> Either ReadError [Form]
readForms inputName = runParse inputName (many readForm)

-- | Converts a list of forms into a String representation. The resulting string
-- will consist of legal Cantor syntax and will be able to be parsed by
-- readForms.
showForms :: [Form] -> String
showForms forms = List.intercalate "\t" $ map showForm forms

showForm :: Form -> String
showForm (Int i)   = show i
showForm (Float f) = show f
showForm (Ident s) = s

-- | The type for Cantor parsers (indentation-sensitive)
type IParser a = ParsecT String () (Control.Monad.State.State SourcePos) a

-- | Uses the specified parser to parse a string, returning the result or a
-- ParseError.
runParse :: InputName -> IParser a -> String -> Either ParseError a
runParse source aParser input = Parsec.Indent.runIndent source run
  where run = runParserT aParser () source input

-- | Parses a single Form from the input stream.
readForm :: IParser Form
readForm = identifier <|>
           intOrFloat <|>
           initialHyphen

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
  c  <- letter
  cs <- many (alphaNum <|> oneOf identSymbols)
  skipSpaces
  return $ Ident $ c:cs

exponentPart :: IParser String
exponentPart = do
    char 'e'
    minus <- option "" (string "-")
    int   <- many1 digit
    return $ "e" ++ minus ++ int

-- | Parses the fractional part of a float and returns a Form by appending the
-- provided string to the result and converting to a Double.
fraction :: String -> IParser Form
fraction s = do
  char '.'
  digits  <- many1 digit
  expPart <- option "" exponentPart
  return $ Float $ read $ s ++ "." ++ digits ++ expPart

-- | Parses either an Int or a Float, depending on whether a '.' character is
-- found in the stream.
intOrFloat :: IParser Form
intOrFloat = do
  int  <- many1 digit
  form <- option (Int (read int)) (fraction int)
  skipSpaces
  return form

initialHyphen :: IParser Form
initialHyphen = do
  char '-'
  num <- intOrFloat
  return $ case num of
    Int i   -> Int $ negate i
    Float f -> Float $ negate f
    _       -> error "Unexpected return from intOrFloat"
