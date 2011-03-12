{-# LANGUAGE TemplateHaskell #-}
module Ex4StateParser where

import Data.Parser.Grempa.Static
import Data.Parser.Grempa.Dynamic
import Control.Monad.State hiding (state)

-- Import the grammar.
import Ex4StateB
-- We also need the token datatype in scope or Template Haskell will complain.
import Ex4StateLex

parseStateStatic :: Parser Tok Expr
parseStateStatic = $(mkStaticParser state [|state|])

-- | Combine the lexer with a parser
lexAndParse :: String -> Expr
lexAndParse = parse parseStateStatic . lexToks

-- | Try it out!
test :: [Expr]
test = map lexAndParse inputString
  where
    inputString = [ "\\x -> (\\y -> y) x"
                  , "\\x -> \\y -> \\z -> x y z"
                  ]
