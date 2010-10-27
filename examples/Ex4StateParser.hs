{-# LANGUAGE TemplateHaskell #-}
module Ex4StateParser where

import Data.Parser.Grempa.Static
import Data.Parser.Grempa.Dynamic
import Control.Monad.State

-- Import the grammar.
import Ex4State
-- We also need the token datatype in scope or Template Haskell will complain.
import Ex4StateLex

parseStateStatic :: Parser Tok (St Expr)
parseStateStatic = $(mkStaticParser fun [|fun|])

-- | Combine the lexer with a parser
lexAndParse :: String -> Expr
lexAndParse = evalSt . parse parseStateStatic . lexToks

-- | Try it out!
test :: [Expr]
test = map lexAndParse inputString
  where
    inputString = [ "\\x -> (\\y -> y) x"
                  , "\\x -> \\y -> \\z -> x y z"
                  ]
