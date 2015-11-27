-- Skridskor
-- Problem ID: skridskor Time limit: 1 second Memory limit: 1024 MB

-- Input: heltal R och C; sedan R rader med C tecken (’.’ = tom, ’#’ = hinder)

-- Mål: åka ut åt öster.
-- Villkor Om hon åker ut på någon annan sida av rinken (uppe, nere eller till vänster) så misslyckas hon med sitt mål.
-- Start: Natalie börjar alltid på ruta (0,0) och åker åt höger.

-- Output: det minsta antal svängar Natalie behöver göra för att ta sig ut från isen på höger sida.

import qualified Data.Map as Map
import Data.Map (Map)

type Rad = Int
type Kolumn = Int
type Pos = (Rad, Kolumn)
type Hinder = Bool
type Karta = (Pos, Map Pos Hinder)
data Riktning = Ö | N | V | S
  deriving (Eq, Show)

ettSteg :: Riktning -> Pos -> Pos
ettSteg Ö (r, k) = (r,   k+1)
ettSteg N (r, k) = (r+1, k  )
ettSteg V (r, k) = (r,   k-1)
ettSteg S (r, k) = (r-1, k  )

-- Viktiga operationer:

type UtökadPos = Either Riktning Pos
  -- Left r betyder utanför kartan i riktning r
  -- Right pos är en vanlig position på kartan

-- kollar om en utökad position är farlig (bryter mot villkoret)
farlig :: UtökadPos -> Bool
farlig (Left rikt)  = rikt /= Ö
farlig (Right _pos) = False

åk :: Karta -> Riktning -> Pos -> UtökadPos
åk karta rikt pos | utanför karta nästa  = Left rikt
                  | hinder  karta nästa  = Right pos
                  | otherwise            = åk karta rikt nästa
  where nästa = ettSteg rikt pos

utanför :: Karta -> Pos -> Bool
utanför ((maxrad, maxkol), _) (r, k) =   r < 0 || r >= maxrad
                                     ||  k < 0 || k >= maxkol

hinder :: Karta -> Pos -> Bool
hinder (_, karta) p = Map.lookup p karta == Just True

-- sök :: Karta -> Pos -> Rikt -> UtökadPos
