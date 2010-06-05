{-# LANGUAGE TemplateHaskell #-}

--import Data.Foldable
import Language.Haskell.TH

revFun :: [Bool] -> ExpQ -> ExpQ
revFun = revFun' []
  where
    revFun' bs (a:as) e = do
        x <- newName "x"
        let p  = varP x
            e' = lam1E p e
        revFun' (if a then bs ++ [varE x] else bs) as e'
    revFun' bs [] e = foldr appE e bs


