-- https://open.kattis.com/problems/towers
-- First attempt: too slow
module Main where
import Data.List (sortBy, intersperse)
import Data.Function (on)

type PowerTower = (Integer, [Integer])

main = do
  n <- readLn
  lines <- sequence (replicate n getLine)
  let pts = map line2PowerTower lines
  putStr (solution pts)

solution :: [PowerTower] -> String
solution = unlines . ("Case 1:":) . map showPT . sortOn fst

-- This helper function is in Data.List now but was not in the version used by Kattis (ghc-7.6.3)
sortOn :: Ord b => (a->b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)

showPT :: PowerTower -> String
showPT (_, is) = concat (intersperse "^" (map show is))

line2PowerTower :: String -> PowerTower
line2PowerTower s = (foldr (^) 1 is, is)
  where is = map read (splitBy '^' s)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy sep xs | null first = []
               | otherwise  = first : splitBy sep (drop 1 rest)
  where
    (first, rest) = span (sep/=) xs
