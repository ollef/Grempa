module Data.Parser.Grempa.Parser.Error where

import Data.List

import Data.Parser.Grempa.Grammar.Token

type ParseResult t a = Either (ParseError t) a

type Parser t a = [t] -> ParseResult t a

-- | The different kinds of errors that can occur
data ParseError t
    = ParseError
        { expectedTokens :: [Tok t]
        , position       :: Integer
        }
    | InternalParserError
        { position :: Integer }
  deriving Show

instance Functor ParseError where
    fmap f (ParseError e p)        = ParseError (map (fmap f) e) p
    fmap _ (InternalParserError p) = InternalParserError p

-- | Make default error strings
showError :: Show t => ParseError t -> String
showError e = case e of
    ParseError ts pos       -> "Parse error at " ++ show pos
                            ++ ", expecting one of {"
                            ++ intercalate "," (map tokToString ts) ++ "}."
    InternalParserError pos -> "Internal parser error at "
                            ++ show pos ++ "."

parse :: Show t => Parser t a -> [t] -> a
parse p i = case p i of
    Left err  -> error $ showError err
    Right res -> res
