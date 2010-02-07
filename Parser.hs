{-# LANGUAGE NoImplicitPrelude #-}
module Parser where
import qualified Prelude as P
import ParserChecked as PC
import Data.Char

-- Helper functions
--munch :: (s -> P.Bool) -> CParser s [s]
munch r = do s <- look; inspect s
  where
    inspect (c:cs) | r c = do symbol; cs' <- inspect cs; PC.return (c:cs')
    inspect _ = PC.return []


--failer :: PC.CParser s s
failer = failer >> symbol
nonfail = symbol >> nonfail <|> symbol

--failer r = failer r >> symbol
--failer _     = PC.return 'a'
    
