-- This is free and unencumbered software released into the public domain.

-- | Implementation of Cantor's core shared datatypes.
module Datatypes where

import qualified Control.Monad.State as State
import qualified Text.Parsec         as Parsec
import qualified Text.Parsec.Expr as Expr

-- | Forms are the components of the Cantor parse tree. A form can be 1) an
-- atom, such as an identifier or literal, 2) an operator application to other
-- forms, or 3) A Sexp (a list of forms).
data Form = Int Integer
          | Float Double
          | Str String
          | Ident String
          | Sexp [Form] 
          | Binop String Form Form
            deriving (Eq, Show, Read)

makeVector :: [Form] -> Form
makeVector = Sexp . (Ident "Vector" :)

makeMap :: [Form] -> Form
makeMap = Sexp . (Ident "Map" :)

makeDot :: Form -> Form -> Form
makeDot left right = Sexp [Ident "dot", left, right]

makeSubscript :: Form -> Form -> Form
makeSubscript left right = Sexp [Ident "at", left, right]
            
-- | Indicates a failure during parsing.
type ReadError = Parsec.ParseError

-- | The name of a parser input
type InputName = String

-- | Which characters the 'skipSpaces' parser should treat as whitespace
-- and thus skip.  
newtype SkippedWhitespace = SkippedWhitespace String

-- | The type for Cantor parsers (indentation-sensitive)
type CantorParser a = Parsec.ParsecT
                        String
                        SkippedWhitespace
                        (State.State Parsec.SourcePos)
                        a

-- | The type for Cantor operator parsers
type CantorOperator = Expr.Operator
                        String
                        SkippedWhitespace
                        (State.State Parsec.SourcePos)
                        Form

-- | The type for a table of Cantor operators
type CantorOperatorTable = [[CantorOperator]]