-- Hesthoppning
-- Problem ID: hesthoppning Time limit: 7 seconds Memory limit: 1024 MB
-- Input: första raden: rutans storlek: N och M, separerade med ett blanksteg.
-- N rader med M tecken: Ett ’.’= tom, ’#’ = sten, ’H’ = hest
-- Hagen är omgiven av stängsel. Det är garanterat att indata alltid innehåller exakt två ’H’-celler.
-- Output: Ditt program ska skriva ut ett ord på en rad - "JA" om hestarna kan mötas på någon cell och "NEJ" annars.

import qualified Data.Map as Map
import Data.Map (Map)

data Ruta = Tom | Sten | Hest
  deriving (Eq, Show)
type Rad = Int
type Kolumn = Int
type Pos = (Rad,Kolumn)
type Karta = Map Pos Ruta

-- Det räcker att ta reda på om den första hästen kan nå den andra
-- hästen. Brädden-först-sökning av alla möjliga vägar verkar
-- lämpligt, men man bör nog hålla reda på redan besökta rutor under
-- tiden för att undvika onödiga beräkningar.

--
kanNåsFrån karta p1 p3 = or (map (\i -> kanNåsFrånSteg karta i p1 p3) [0..n])
  where ks = Map.keys karta
        n  = maxRad * maxKolumn  -- en "svag" övre gräns för sökningen
        maxRad    = maximum (map fst ks)
        maxKolumn = maximum (map snd ks)


kanNåsFrånSteg karta 0 p1 p3 = p1 == p3
kanNåsFrånSteg karta n p1 p3 = any (\p2 -> kanNåsFrånSteg karta (n-1) p2 p3) (hesthoppFrån karta p1)

hesthoppFrån karta p = [p2 | p2 <- allaHestHoppFrån p, Map.findWithDefault Sten p2 karta /= Sten]

allaHestHoppFrån (r,k) = map (\(a,b)->(r+a,k+b)) [(2,1),(2,-1),(1,2),(1,-2),(-1,2),(-1,-2),(-2,1),(-2,-1)]

byggKarta :: [String] -> Karta
byggKarta css = Map.fromList (concat qq)
  where  q :: [(Rad,String)]
         q = numrera css
         qq :: [[(Pos,Ruta)]]
         qq = map (\(r,cs) ->
                      map (\(k,c) -> ((r,k),char2Ruta c))
                          (numrera cs))
                  q

char2Ruta :: Char -> Ruta
char2Ruta 'H' = Hest
char2Ruta '.' = Tom
char2Ruta '#' = Sten
char2Ruta c   = error ("char2Ruta: otillåtet tecken: " ++ [c])


numrera = zip [1..]


test1 = ["H.H","...",".#."]
test2 = ["H#H","...",".#."]

{-
H.H
...
.#.

H.H
..1
.#.

H.H
..1
2#.

H3H
..1
2#.

H3H
..1
2#4

H3H
5.1
2#4

H36
5.1
2#4

Klart!

-}
