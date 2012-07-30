-- This is free and unencumbered software released into the public domain.

-- | The Translator module is responsible for translating input Forms into
-- valid C++ syntax.
module Translator (
  TranslationError,
  translate,
) where

import Control.Applicative ((<$>))
import Datatypes
import Data.Map (Map, member, (!), fromList)
import Reader (showForms)

-- | Accepts as input a Form and returns either a valid C++ string or a
-- TranslationError with details on why such a translation is impossible.
translate :: Form -> Either TranslationError String
translate (Int i) = Right $ show i
translate (Float f) = Right $ show f
translate (Ident i) = Right i
translate (Str s) = Right $ show s
translate (Exp (Ident i) fs) | i `member` builtins = (builtins ! i) fs
translate _ = error "todo implement this"

-- | The type of all functions that implement builtins (operations which
-- are impossible to define in terms of Cantor itself).
type Builtin = [Form] -> Either TranslationError String

-- | Maps names to functions implementing the Cantor builtins.
builtins :: Map String Builtin
builtins = fromList [
    ("cpp", translateCpp)
  ]

-- | Embeds literal c++ code. The 'cpp' builtin should either be applied to a
-- single string or to a form headed by the identifier 'str'.
translateCpp :: Builtin
translateCpp [(Str s)] = Right s
translateCpp [(Exp (Ident "str") fs)] = concat <$> mapM translateStr fs
translateCpp forms = Left $ TranslationError err
  where err = "Invalid arguments to cpp builtin:\n" ++ showForms forms
