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

-- Några hjälpfunktioner för I/O

fråga :: Read a => String -> IO a
fråga text = do
  putStr text
  readLn

frågor :: Read a => Int -> String -> IO [a]
frågor n text = mapM (\i -> fråga (text ++ show i ++ " ? ")) [1..n]

svar :: Show a => a -> IO ()
svar x = putStrLn ("Svar: " ++ show x)
