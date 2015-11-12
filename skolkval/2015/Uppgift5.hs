-- Uppgift 5 – Plocka äpplen

import Data.Map
import Prelude as P
import Data.List
import Lib

type N = Int
type K = Int
testcases :: [((N, K, [Äpplen], [Äpplen]),   Äpplen)]
testcases =
--  ((n, k, norraRaden,    södraRaden),   svar)
  [ ((2, 2, [7, 8],        [6, 4]),         13)
  , ((3, 7, [5, 5, 5],     [5, 5, 5]),      30)
  , ((4, 6, [9, 7, 5, 11], [3, 14, 6, 8]),  48)
  ]

data Riktning = Öster | Norr | Väster | Söder
  deriving Show
type Stig = [Plats]

type Nord = Int -- Från söder = 1 till norr = 2
type Ost  = Int -- Från väster = 1 till längst åt öster = n
type Plats = (Ost, Nord)

-- Fakta: Hon börjar med första trädet i södraRaden.
startPlats :: Plats
startPlats = (1,1) -- sydväst

norraRadensPlatser :: [Plats]
norraRadensPlatser = [(x, 2) | x <- [1..]]
södraRadensPlatser :: [Plats]
södraRadensPlatser = [(x, 1) | x <- [1..]]

-- | Ta ett steg (om möjligt)
steg :: N -> Plats -> Riktning -> Maybe Plats
steg n (x, y) Öster  | x < n = Just (x+1, y)
steg _ (x, y) Norr   | y < 2 = Just (x, y+1)
steg _ (x, y) Väster | 1 < x = Just (x-1, y)
steg _ (x, y) Söder  | 1 < y = Just (x, y-1)
steg _ _      _              = Nothing

type Äpplen = Int -- Antal äpplen på ett "oplockat" träd
type Karta = Map Plats Äpplen

raderTillKarta :: [Äpplen] -> [Äpplen] -> Karta
raderTillKarta nR sR = fromList (zip norraRadensPlatser nR ++
                                 zip södraRadensPlatser sR)

plockaEttTräd :: Plats -> Karta -> (Äpplen, Karta)
plockaEttTräd plats karta = (äpplen, nyKarta)
  where äpplen  = findWithDefault 0 plats karta
        nyKarta = Data.Map.delete plats karta -- Ta bort trädet från kartan.

-- Uppgift: Beräkna bästa lösning i k steg från plats p på en viss karta ka.
-- Önsketänkande: vi vet bästa lösningen i (k-1) steg från alla grannplatser
-- Då väljer vi bara den bästa av dessa och lägger till äpplen från vår plats.

bäst1 :: N -> Karta -> K -> Plats -> Äpplen
bäst1 n ka 0 p = 0
bäst1 n ka k p = äpplen + maximum (P.map (bäst1 n nyKarta (k-1)) grannplatser)
  where grannplatser = [pl | Just pl <- P.map (steg n p) allaRikt]
        (äpplen, nyKarta) = plockaEttTräd p ka
-- TODO: Det kan bli många onödiga sökvägar och därför ta mycket tid
--   men för testfallen fungerar det bra.

-- | Samma algoritm, men sparar också stigen.
bäst :: Int -> Karta -> Int -> Plats -> (Äpplen, Stig)
bäst n ka 0 p = (0, [])
bäst n ka k p = (äpplen + restÄpplen, p:stig)
  where grannplatser = [pl | Just pl <- P.map (steg n p) allaRikt]
        (äpplen, nyKarta) = plockaEttTräd p ka
        alternativ = sortBy jmf (P.map (bäst n nyKarta (k-1)) grannplatser)
        (restÄpplen, stig) : _ = alternativ

jmf :: (Äpplen, a) -> (Äpplen, a) -> Ordering
jmf (a, _) (b, _) = compare b a

allaRikt = [Öster, Norr, Väster, Söder]

solution' :: (N, K, [Äpplen], [Äpplen]) -> (Äpplen, Stig)
solution' (n, k, nR, sR) = bäst n karta k startPlats
  where karta = raderTillKarta nR sR

solution = fst . solution'

runTests sol = all (\(inp, outp) -> sol inp == outp) testcases
test = runTests solution

main = do
  n <- fråga "Antal träd per rad ? "
  k <- fråga "Antal hon hinner besöka ? "
  nR <- frågor n "Norra raden, träd "
  sR <- frågor n "Södra raden, träd "
  svar (solution (n, k, nR, sR))



----------------------------------------------------------------
-- Onödiga varianter

bästIO :: Int -> Karta -> Int -> Plats -> IO (Äpplen, Stig)
bästIO n ka 0 p = return (0, [])
bästIO n ka k p = do
  let grannplatser = [pl | Just pl <- P.map (steg n p) allaRikt]
      (äpplen, nyKarta) = plockaEttTräd p ka
  print (k, p, grannplatser, nyKarta) -- ifall man vill lite av vad som pågår
  alternativ <- fmap (sortBy jmf) (sequence (P.map (bästIO n nyKarta (k-1)) grannplatser))
  let (restÄpplen, stig) : _ = alternativ
  return (äpplen + restÄpplen, p:stig)



{-
karta :: Int -> Karta
karta i = raderTillKarta nR sR
  where (_, _, nR, sR) = fst (testcases !! i)

visaKarta :: Karta -> [[Äpplen]]
visaKarta karta = [ P.map slåUpp (take n norraRadensPlatser)
                  , P.map slåUpp (take n södraRadensPlatser)
                  ]
  where n = maximum (P.map snd (keys karta))
        slåUpp plats = findWithDefault 0 plats karta

type VisaVäg = [Riktning]

visadVägTillStig :: N -> VisaVäg -> Plats -> Maybe Stig
visadVägTillStig _ []     p = Just [p]
visadVägTillStig n (r:rs) p = do
  nyPlats <- steg n p r
  visadVägTillStig n rs nyPlats

-}
