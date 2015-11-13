-- Uppgift 7 – Regering
import Data.List

testcases :: [([Int], Int)]
testcases =
  [ ([3, 5, 8, 4, 2], 4)
    -- Förklaring: Regeringen måste ha minst 12 mandat för majoritet. De fyra möjliga regeringarna har: 3 + 5 + 4 mandat, 5 + 8 mandat, 8 + 4 mandat, 3 + 8 + 2 mandat
  , ([12, 66, 39, 37, 21, 31, 20, 53, 20, 6], 71)
  ]

-- Naiv lösning (tar för mycket tid för stora indata)
solution = length . solution'

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

runTests sol = all (\(inp, outp) -> sol inp == outp) testcases
test = runTests solution

-- Inte en färdig lösning
