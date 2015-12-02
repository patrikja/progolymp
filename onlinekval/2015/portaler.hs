import Data.Set hiding (map, filter)
import Data.Array
-- Portaler
-- Problem ID: portaler Time limit: 1 second Memory limit: 1024 MB
-- Input: heltalet N, antalet portaler. Sedan följer N rader med tal 1≤di≤N som beskriver att portalen på rad i leder till portal di. Portalerna är ett-indexerade (den lägsta har nummer 1, den högsta har nummer N), och kommer i ordning från 1 till N i indata.
-- Sedan följer en rad med ett heltal Q, antalet förfrågningar som du måste svara på. Efter det följer Q rader med två heltal 1≤s,e≤N. Dessa tal beskriver nummer på en startportal s och en slutportal e, och ditt uppdrag är att svara på hur många gånger man behöver gå igenom portalerna för att ta sig till e när man börjar vid s. För varje förfrågan gäller s≠e.
-- Output: Du ska skriva ut Q rader, ett tal per förfrågan: antalet gånger man behöver gå igenom portalerna för att ta sig från den givna startportalen till slutportalen. Om det är omöjligt så skriver du ut −1. Skriv ut svaret för varje förfrågan på en separat rad.

testinp :: ([Nod], [(Nod,Nod)])
testinp  = ( [2,3,4,3,4]     -- ds
           , [(1, 2), (1, 4), (2, 5)] -- par
           )
testout = [1, 3, -1]

byggGraf :: [Int] -> Array Int [Int]
byggGraf ds = listArray (1,length ds) (map (:[]) ds)

solution :: [Int] -> [(Int, Int)] -> [Int]
solution ds par = map (test ds) par
-- TODO: Utnyttja vi lär oss mer om grafen för varje indata (kan bli effektivare)

-- Sök kortaste vägen i en enkel riktad graf (undvik att hamna i cykler).

-- Bredden-först-sökning av ett sökproblem från en nod ger en lista
-- med noder parat med avståndet från startnoden.

type Sökproblem a = a -> [a]

bfs :: Ord a => Sökproblem a -> a -> [(Int, a)]
bfs sp = bfs' sp empty . sing

sing :: a -> [(Int, a)]
sing x = [(0,x)]

bfs' :: (Num i, Ord a) =>
  Sökproblem a -> Set a -> [(i, a)] -> [(i, a)]
bfs' nästa visited [] = []
bfs' nästa visited ((level, node):lns)
  | member node visited = bfs' nästa visited lns
  | otherwise = (level, node) : bfs' nästa v' (lns ++ nexts)
      where nexts = map ((,) (level+1)) (nästa node)
            v' = insert node visited

type Vinst a = a -> Bool

test ds (start, slut) = check (filter ((slut==) . snd) (bfs nästa start))
  where  graf = byggGraf ds
         nästa = (graf !)

type Avstånd = Int
type Nod = Int
check :: [(Avstånd, Nod)] -> Avstånd
check [] = -1
check ((avs, _):_) = avs

main = do
  n   <- readLn
  ds  <- sequence (replicate n readLn)
  q   <- readLn
  par <- sequence (replicate q getPair)
  let svar = solution ds par
  sequence (map print svar)

getPair :: IO (Int, Int)
getPair = do
  s <- getLine
  let from:to:_ = words s
  return (read from, read to)
