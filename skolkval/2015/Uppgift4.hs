-- Uppgift 4 – Kaninhål
import Lib

type Bäver = Int
type Djup = Int
testcases :: [([Bäver], [Djup])]
testcases = [ ([3,4,2,1,5], [1,1,2])
            , ([3,2,1,4,6,7,5], [5,4,4])
            ]

-- Representation: Låt en "bäver-rad" representeras av en lista med
-- första bävern vid listans huvud istället för som i uppgiften. (Vi
-- kan vända på listan i samband med in- och utmatning.)

framåtEttHål :: Djup -> [Bäver] -> [Bäver]
framåtEttHål d bs = resten ++ reverse iHålet
  where iHålet = take d bs
        resten = drop d bs

framåtFleraHål :: [Djup] -> [Bäver] -> [Bäver]
framåtFleraHål []      bs = bs -- inga hål => ingen ändring
framåtFleraHål (d:ds)  bs = framåtFleraHål ds (framåtEttHål d bs)

-- Med djupen ds hur blir slutraden enligt uppgiften om man börjar med n bävrar?
framåt :: [Djup] -> Int -> [Bäver]
framåt ds n = reverse (framåtFleraHål ds [1..n])

-- Huvuduppgift: sök djupen så att framåt ds n == önskadRad

solution :: [Bäver] -> [Djup]
solution = head . allaLösningar

allaLösningar :: [Bäver] -> [ [Djup] ]
allaLösningar önskadRad = [ds | ds <- allaDjup, framåt ds n == önskadRad]
  where n = length önskadRad
        allaDjup = [ [d1, d2, d3] | d1 <- [1..n-1], d2 <- [1..n-1], d3 <- [1..n-1] ]

runTests sol = all (\(inp, outp) -> sol inp == outp) testcases
test = runTests solution

main = do
  bs <- getList  "Antal bävrar ? "
                 "Nummer ? "
  let [d1, d2, d3] = solution bs
  putStrLn $ "Hålens djup: " ++ show d1 ++ " "
                             ++ show d2 ++ " "
                             ++ show d3

{- Kommentar: Det kanske går att hitta ett "smart" sätt att lösa
problemet, men eftersom uppgiften begränsar antalet bävrar till max 10
blir det mindre än 1000 fall att testa vilket inte är något problem
för körtiden.
-}
