{-# LANGUAGE TemplateHaskell #-}
module Fun2 where
import Fun
import Lex

import Data.Parser.Grempa.Dynamic
import Data.Parser.Grempa.Static

test :: Parser Tok [Def]
test = mkDynamicParser constrWrapper lang

test2 :: Parser Tok [Def]
test2 = $(mkStaticParser lang [|lang|])

