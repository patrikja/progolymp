-- Kent-buggen
-- Problem ID: kentbuggenTime limit: 6 secondsMemory limit: 1024 MB
-- Indata: n (1≤n≤10^5) - antalet artister. Sedan följer n rader med namn.
-- Utdata: antalet olika artistnamn som förekommer mer än en gång.

import qualified Data.Map.Strict as Map

type Namn = String

solution :: [Namn] -> Int
solution = kollaTabell . fyllTabell

type Antal = Int
type Tabell = Map.Map String Antal

fyllTabell :: [Namn] -> Tabell
fyllTabell = foldr f Map.empty
  where f n tab = Map.insertWith (+) n 1 tab

kollaTabell :: Tabell -> Int
kollaTabell = length . filter ((>1).snd) . Map.assocs

main = do
  antal <- readLn
  ns <- sequence (replicate antal getLine)
  print (solution ns)
