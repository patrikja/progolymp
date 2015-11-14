-- https://po.kattis.com/problems/apples
-- (nästan samma uppgift som skolkval 2015 uppgift 5)

import qualified Data.Map as Map
import Data.Map (Map)
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
raderTillKarta nR sR = Map.fromList (zip norraRadensPlatser nR ++
                                     zip södraRadensPlatser sR)

plockaEttTräd :: Plats -> Karta -> (Äpplen, Karta)
plockaEttTräd plats karta = (äpplen, nyKarta)
  where äpplen  = Map.findWithDefault 0 plats karta
        nyKarta = Map.delete plats karta -- Ta bort trädet från kartan.

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


strängTillLista :: Read a => String -> [a]
strängTillLista = map read . words
läsFleraTal :: Read a => IO [a]
läsFleraTal = do str <- getLine
                 return (strängTillLista str)
main :: IO ()
main = do
  [n, k] <- läsFleraTal
  nR <- läsFleraTal
  sR <- läsFleraTal
  print (solution (n, k, nR, sR))
