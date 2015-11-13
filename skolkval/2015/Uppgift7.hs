-- Uppgift 7 – Regering
import Data.List
import Lib

testcases :: [([Int], Int)]
testcases =
  [ ([3, 5, 8, 4, 2], 4)
    -- Förklaring: Regeringen måste ha minst 12 mandat för majoritet. De fyra möjliga regeringarna har: 3 + 5 + 4 mandat, 5 + 8 mandat, 8 + 4 mandat, 3 + 8 + 2 mandat
  , ([12, 66, 39, 37, 21, 31, 20, 53, 20, 6], 71)
  ]

{- Generera (ett träd med) alla delmängder och välj de som uppfyller kraven.

Definiera ett träd som i varje nod gör ett val om ett parti ska
utelämnas (vänster) eller tas med (höger). Under uppbyggnade håller vi
också reda på summan så här långt. I varje nod där summan ännu är
lägre en majoritet fortsätter uppdelningen. Där summan når majoritet
kan trädet sluta växa eftersom varje ytterligare utökning bryter mot
villkoret att lösningen är minimal. Det första noden som är "över
gränsen" kan dock ligga onödigt långt över så det gäller att ta
besluten i rätt ordning. Exempel: Om man lägger till först 3, sedan 5
(fortfarande under gränsen) och sist 8 (nu för mycket över gränsen) så
är inte lösningen minimal efters 3 skulle kunna tas bort utan att
äventyra majoritetsställningen.

Men om man börjar ta besluten från det största partiet och sedan går
nedåt kommer man inte riskera att "kliva för långt över
majoritetsgränsen".

-}

type Mandat = Int
type Sum = Int
data Tree a = Leaf a
            | Node (Tree a) (Tree a)
            | Fail a -- värdet av typ a behövs inte men hjälper vid felsökning
  deriving Show

majoritet :: [Mandat] -> Mandat
majoritet ms = knapptHalva + 1
  where
    totMandat    = sum ms
    knapptHalva  = totMandat `div` 2

byggTräd :: [Mandat] -> Tree ([Mandat], Sum)
byggTräd ms = go (reverse (sort ms)) [] 0
  where
    major = majoritet ms
    go ms     msin summa | summa >= major = Leaf (msin, summa)
    -- Om summan är för liten
    go []     msin summa = Fail (msin, summa)
    go (m:ms) msin summa = Node  (go ms    msin     summa )
                                 (go ms (m:msin) (m+summa))

flatten :: Tree a -> [a]
flatten (Leaf x)   = [x]
flatten (Node l r) = flatten l ++ flatten r
flatten (Fail x)   = []

solution :: [Mandat] -> Int
solution = length . flatten . byggTräd

main = do
  n <- fråga "Antal partier ? "
  ms <- frågor n "Parti "
  svar (solution ms)

runTests sol = all (\(inp, outp) -> sol inp == outp) testcases
test = runTests solution


----------------------------------------------------------------

-- Naiv lösning (tar för mycket tid för stora indata)
solutionNaiv = length . solution'

solution' ms = filter (ejFörStor majoritet) (concat allaKandidater)
  where totMandat    = sum ms
        knapptHalva  = totMandat `div` 2
        majoritet | even totMandat = knapptHalva + 1
                  | otherwise      = knapptHalva
        allaKandidater = [f ms i | i <- [majoritet .. totMandat]]


-- f ms m = alla sätt att få från listan ms få exakt m mandat
f ms m | m <  0     = []    -- omöjligt
       | m == 0     = [[]]  -- en lösning: den tomma listan
       | otherwise  = nubsort (concat lösAllaMindreProblem)
  where -- för varje k i ms: samla ihop alla lösningar utan k + lägg in k
        lösAllaMindreProblem = [ map (insert k) (f (ms \\ [k]) (m-k))
                               | k <- ms]

nubsort (x:xs) = nubsort smaller ++ x : nubsort bigger
  where smaller = filter (<x) xs
        bigger  = filter (>x) xs
nubsort xs = xs

ejFörStor :: Int -> [Int] -> Bool
ejFörStor majoritet ms = all (\m -> sum ms - m < majoritet) ms
