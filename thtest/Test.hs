module Test where

import Language.Haskell.TH

list = [1, 2, 3] 

mkCase list = do
    var <- newName "n"
    return $ LamE [VarP var] $ 
        CaseE (VarE var) $
            map (\n -> Match 
                (LitP (IntegerL n)) 
                (NormalB (LitE (IntegerL (n+1)))) 
                []) 
                list
  
