module Lib where

-- | Hjälpfunktion för inmatning av lista (inte en del av just den här
-- uppgiften, men ofta användbar i dessa tävlingar).
getList :: Read a => String -> String -> IO [a]
getList frågaAntal frågaVarje = do
  putStr frågaAntal
  n <- readLn
  sequence $ replicate n $ do
    putStr frågaVarje
    readLn
