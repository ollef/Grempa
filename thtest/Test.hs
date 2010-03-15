{-# LANGUAGE TemplateHaskell #-}
module Test where

import Language.Haskell.TH

data Symbol = Empty | Symbol String

list = [Symbol "a", Symbol "b", Empty] 

mkCase list = do
    var <- newName "n"
    m <- mapM (\x -> Match [p|x|] (NormalB (LitE (IntegerL (1)))) []) list
    return $ LamE [VarP var] $ 
        CaseE (VarE var) m
