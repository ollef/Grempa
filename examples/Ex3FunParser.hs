{-# LANGUAGE TemplateHaskell #-}
module Ex3FunParser where

import Data.Parser.Grempa.Static
import Data.Parser.Grempa.Dynamic

-- Import the grammar.
import Ex3Fun
-- We also need the token datatype in scope or Template Haskell will complain.
import Ex3FunLex

-- | Make a static parser
parseFunStatic :: Parser Tok [Def]
parseFunStatic = $(mkStaticParser fun [|fun|])

-- | Make a dynamic parser using 'constrWrapper'.
parseFunDynamic :: Parser Tok [Def]
parseFunDynamic = mkDynamicParser constrWrapper fun

-- | Combine the lexer with a parser
lexAndParse :: String -> [Def]
lexAndParse = parse parseFunStatic . lexToks

-- | Try it out!
test :: [[Def]]
test = map lexAndParse inputString
  where
    inputString = [ "f (X x) = Y x; g x y z = x * y + z"
                  , "fromJust m = case m of {Just x -> x; Nothing -> undefined}"
                  , "foldr f s (Cons x xs) = f x $ foldr f s xs; foldr f s Nil = s"
                  ]
