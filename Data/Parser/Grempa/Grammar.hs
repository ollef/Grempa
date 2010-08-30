-- | Grammar construction toolkit.
{-# LANGUAGE DoRec, TypeFamilies #-}
module Data.Parser.Grempa.Grammar
    ( module Data.Parser.Grempa.Grammar.Typed
    , several, severalInter
    ) where
import Data.Typeable
import Data.Parser.Grempa.Grammar.Typed
    (Grammar, rule, ToSym(..), (<#>), (<#), (<@>), (<@), epsilon)

-- | Create a new rule which consists of any number of the argument rule.
--   Example: @several rule@ matches @rule rule ... rule@
several :: (ToSym s x, ToSymT s x ~ a, Typeable a, Typeable s)
        => x -> Grammar s [a]
several x = do
  rec
    xs <- rule [epsilon []
               ,(:) <@> x <#> xs]
  return xs

-- | Create a new rule which consists of a list interspersed with a token.
--   Example: @severalInter ';' rule@ matches @rule ';' rule ';' ... ';' rule@
severalInter :: (ToSym s x, ToSymT s x ~ a, Typeable a, Typeable s)
             => s -> x -> Grammar s [a]
severalInter tok x = do
  rec
    xs <- rule [epsilon []
               ,(:[]) <@> x
               ,(:)   <@> x <# tok <#> xs]
  return xs
