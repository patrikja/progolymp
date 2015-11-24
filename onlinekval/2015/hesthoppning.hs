-- Hesthoppning
-- Problem ID: hesthoppning Time limit: 7 seconds Memory limit: 1024 MB
-- Input: första raden: rutans storlek: N och M, separerade med ett blanksteg.
-- N rader med M tecken: Ett ’.’= tom, ’#’ = sten, ’H’ = hest
-- Hagen är omgiven av stängsel. Det är garanterat att indata alltid innehåller exakt två ’H’-celler.
-- Output: Ditt program ska skriva ut ett ord på en rad - "JA" om hestarna kan mötas på någon cell och "NEJ" annars.

import qualified Data.Map as Map
import Data.Map (Map)

data Ruta = Tom | Sten | Hest | Besökt
  deriving (Eq, Show)
type Rad = Int
type Kolumn = Int
type Pos = (Rad,Kolumn)
type Karta = Map Pos Ruta

-- Tillåtna hesthopp från en viss punkt
hesthoppFrån :: Karta -> Pos -> [Pos]
hesthoppFrån karta p = [p2 | p2 <- allaHestHoppFrån p, braPlats karta p2]

braPlats karta p2 = Map.findWithDefault Sten p2 karta == Tom

-- Alla åtta "hesthopp" från en viss punkt
allaHestHoppFrån :: Pos -> [Pos]
allaHestHoppFrån (r,k) = map (\(a,b)->(r+a,k+b)) [ ( 2,1), ( 2,-1)
                                                 , ( 1,2), ( 1,-2)
                                                 , (-1,2), (-1,-2)
                                                 , (-2,1), (-2,-1)
                                                 ]

byggKarta :: [String] -> Karta
byggKarta css = Map.fromList (concat numreradePlatser)
  where  numreradeRader :: [(Rad,String)]
         numreradeRader = numrera css
         numreradePlatser :: [[(Pos,Ruta)]]
         numreradePlatser = map (\(r,cs) -> paraMed r (numrera cs)) numreradeRader
         paraMed r = map (\(k, c) -> ((r, k), char2Ruta c))

char2Ruta :: Char -> Ruta
char2Ruta 'H' = Hest
char2Ruta '.' = Tom
char2Ruta '#' = Sten
char2Ruta c   = error ("char2Ruta: otillåtet tecken: " ++ [c])

numrera :: [a] -> [(Int, a)]
numrera = zip [1..]

test1 = ["H.H","...",".#."]
test2 = ["H#H","...",".#."]
test3 = "H..":replicate 100 "..."++["H.."]

----------------------------------------------------------------
-- Utgå från (en karta som visar alla platser nåbara efter n steg) och
-- (en lista på intressanta punkter). Fyll i alla punkter som är
-- nåbara från någon av de intressanta punkterna. Då får vi (kartan
-- med punkter nåbara efter n+1 steg) samt (nya intressanta punkter).

solution :: [String] -> String
solution = jaNej . fst . leta . byggKarta

jaNej :: Bool -> String
jaNej b = if b then "JA" else "NEJ"

leta :: Karta -> (Bool, (Karta, [Pos]))
leta karta = (check resultat, resultat)
  where resultat = head (dropWhile (not.done) (iterate steg startPar))
        startPar = (Map.adjust (const Tom) slutPos karta, [startPos])
        (startPos,Hest):(slutPos,Hest):_ = filter ((Hest==).snd) (Map.assocs karta)
        done (k, ps)  = null ps || check (k, ps)
        check (k, ps) = Map.lookup slutPos k == Just Besökt

steg :: (Karta, [Pos]) -> (Karta, [Pos])
steg (karta, intressant) = (nyKarta, nyaPos)
  where nyaPos = uniq [p | rk <- intressant, p <- hesthoppFrån karta rk]
        nyKarta = foldr (Map.adjust (const Besökt)) karta nyaPos

uniq :: Ord a => [a] -> [a]
uniq (x:xs) = uniq lt ++ x:uniq gt
  where  lt = filter (<x) xs
         gt = filter (>x) xs
uniq xs     = xs

main :: IO ()
main = do
  nOchM <- getLine
  let n, m :: Int
      (n, rest):_ = reads nOchM
      (m, _):_    = reads rest
  råKarta <- sequence (replicate n getLine)
  putStrLn (solution råKarta)
