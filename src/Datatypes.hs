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
          | Map [Form]
          | Vector [Form]
          | Dot Form Form
            deriving (Eq, Show, Read)
            
-- | Indicates a failure during parsing.
type ReadError = Parsec.ParseError

-- | The name of a parser input
type InputName = String

-- | An approach for dealing with newlines encountered during parsing, whether
-- they should be ignored (like spaces and tabs), treated as significant
-- syntax, or whether no whitespace should be skipped at all.
data NewlineStrategy = IgnoreNewlines | NoIgnoreNewlines | NoSkipWhitespace

-- | The type for Cantor parsers (indentation-sensitive)
type CantorParser a = Parsec.ParsecT
                        String
                        NewlineStrategy
                        (State.State Parsec.SourcePos)
                        a

-- | The type for Cantor operator parsers
type CantorOperator = Expr.Operator
                        String
                        NewlineStrategy
                        (State.State Parsec.SourcePos)
                        Form

-- | The type for a table of Cantor operators
type CantorOperatorTable = [[CantorOperator]]