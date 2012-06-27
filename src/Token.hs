-- This is a heavily modified version of the token parsing implementation from
-- Text.Parsec.Token. The original code is used under the license found below.


-- Copyright 1999-2000, Daan Leijen; 2007, Paolo Martini. All rights reserved.
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
-- This software is provided by the copyright holders "as is" and any express or
-- implied warranties, including, but not limited to, the implied warranties of
-- merchantability and fitness for a particular purpose are disclaimed. In no
-- event shall the copyright holders be liable for any direct, indirect,
-- incidental, special, exemplary, or consequential damages (including, but not
-- limited to, procurement of substitute goods or services; loss of use, data,
-- or profits; or business interruption) however caused and on any theory of
-- liability, whether in contract, strict liability, or tort (including
-- negligence or otherwise) arising in any way out of the use of this software,
-- even if advised of the possibility of such damage.

module Token (
  stringLiteral,
  identifier,
  intOrFloat,
  initialHyphen,
  skipSpaces,
  operator,
  highPrecedenceOperator
) where

import           Datatypes
import           Text.Parsec

import qualified Data.Char        as Char
import qualified Text.Parsec.Char as Parsec.Char

-- | Skips whitespace. Based on the current "newline strategy" state, can either
-- skip newlines as well or treat newlines as significant.
skipSpaces :: CantorParser String
skipSpaces = do
  newlineStrategy <- getState
  many $ oneOf $ skipped newlineStrategy
  where skipped IgnoreNewlines   = " \t\n"
        skipped NoIgnoreNewlines = " \t"
        skipped NoSkipWhitespace = ""

-- | Symbols that may legally occur as an identifier as long as they are not the
-- first character.
identSymbols :: String
identSymbols = "!%&*-+=\\|?/<>"

-- | Parses an identifier
identifier :: CantorParser Form
identifier = do
  c  <- letter <?> "identifier"
  cs <- many (alphaNum <|> oneOf identSymbols)
  skipSpaces
  return $ Ident $ c:cs
  
-- | Symbols that may occur as part of an operator
opSymbols :: String
opSymbols = "!%&*-+=\\|?/<>"

-- | Creates a parser which recognizes the operator with the specified name.
operator :: String -> CantorParser (Form -> Form -> Form)
operator name = try $ do
  string name
  notFollowedBy $ (oneOf opSymbols <|> alphaNum)
  skipSpaces
  return $ Binop name

-- | Creates a parser which recognizes the operator with the specified name,
-- designed to be used to parse operators which are higher precedence than
-- function application.
highPrecedenceOperator :: String -> CantorParser (Form -> Form -> Form)
highPrecedenceOperator name = try $ do
  string name
  notFollowedBy $ (oneOf opSymbols)
  skipSpaces
  return $ Binop name

-- | Parses the exponent part of a float in scientific notation.  
exponentPart :: CantorParser String
exponentPart = do
    char 'e'
    minus <- option "" (string "-")
    int   <- many1 digit
    return $ "e" ++ minus ++ int

-- | Parses the fractional part of a float and returns a Form by appending the
-- provided string to the result and converting to a Double.
fraction :: String -> CantorParser Form
fraction s = do
  char '.'
  digits  <- many1 digit
  expPart <- option "" exponentPart
  return $ Float $ read $ s ++ "." ++ digits ++ expPart

-- | Parses either an Int or a Float, depending on whether a '.' character is
-- found in the stream.
intOrFloat :: CantorParser Form
intOrFloat = do
  int  <- many1 digit <?> "number"
  form <- option (Int (read int)) (fraction int)
  skipSpaces 
  return form

-- | Handles a hyphen at the start of a form, which can be the start of a
-- number literal or an operator.
initialHyphen :: CantorParser Form
initialHyphen = do
  char '-' <?> ""
  num <- intOrFloat
  return $ case num of
    Int i   -> Int $ negate i
    Float f -> Float $ negate f
    _       -> error "Unexpected return from intOrFloat"

decimal :: CantorParser Integer
decimal = number 10 digit

number :: Integer -> CantorParser Char -> CantorParser Integer
number base baseDigit = do
  digits <- many1 baseDigit
  let n = foldl (\x d -> base*x + toInteger (Char.digitToInt d)) 0 digits
  seq n (return n)

stringLiteral :: CantorParser Form
stringLiteral = do
  str <- between (char '"') (char '"') (many stringChar) <?> "string literal"
  skipSpaces
  return $ Str $ foldr (maybe id (:)) "" str

stringChar :: CantorParser (Maybe Char)
stringChar = do c <- satisfy stringLetter
                return (Just c)
             <|> stringEscape
             <?> "string character"

stringLetter :: Char -> Bool
stringLetter c = c /= '"' && c /= '\\' && c > '\026'

stringEscape :: CantorParser (Maybe Char)
stringEscape = do{ char '\\'
                 ;     do{ escapeGap  ; return Nothing }
                   <|> do{ escapeEmpty; return Nothing }
                   <|> do{ esc <- escapeCode; return (Just esc) }
                 }

escapeEmpty :: CantorParser Char
escapeEmpty = char '&'

escapeGap :: CantorParser Char
escapeGap = do many1 space
               char '\\' <?> "end of string gap"

escapeCode :: CantorParser Char
escapeCode = charEsc <|> charNum <|> charAscii <|> charControl <?> "escape code"

charControl :: CantorParser Char
charControl = do char '^'
                 code <- upper
                 return (toEnum (fromEnum code - fromEnum 'A'))

charNum :: CantorParser Char
charNum = do code <- decimal
                     <|> do{ char 'o'; number 8 Parsec.Char.octDigit }
                     <|> do{ char 'x'; number 16 Parsec.Char.hexDigit }
             return (toEnum (fromInteger code))

charEsc :: CantorParser Char
charEsc = choice (map parseEsc escMap)
  where parseEsc (c,code) = do{ char c; return code }

charAscii :: CantorParser Char
charAscii = choice (map parseAscii asciiMap)
  where parseAscii (asc,code) = try (do{ string asc; return code })

escMap :: [(Char, Char)]
escMap = zip ("abfnrtv\\\"\'") ("\a\b\f\n\r\t\v\\\"\'")

asciiMap :: [([Char], Char)]
asciiMap = zip (ascii3codes ++ ascii2codes) (ascii3 ++ ascii2)

ascii2codes :: [String]
ascii2codes = ["BS","HT","LF","VT","FF","CR","SO","SI","EM",
               "FS","GS","RS","US","SP"]

ascii3codes :: [String]
ascii3codes = ["NUL","SOH","STX","ETX","EOT","ENQ","ACK","BEL",
               "DLE","DC1","DC2","DC3","DC4","NAK","SYN","ETB",
               "CAN","SUB","ESC","DEL"]

ascii2 :: [Char]
ascii2 = ['\BS','\HT','\LF','\VT','\FF','\CR','\SO','\SI',
          '\EM','\FS','\GS','\RS','\US','\SP']

ascii3 :: [Char]
ascii3 = ['\NUL','\SOH','\STX','\ETX','\EOT','\ENQ','\ACK',
          '\BEL','\DLE','\DC1','\DC2','\DC3','\DC4','\NAK',
          '\SYN','\ETB','\CAN','\SUB','\ESC','\DEL']

