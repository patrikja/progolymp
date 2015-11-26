-- Skridskor
-- Problem ID: skridskor Time limit: 1 second Memory limit: 1024 MB

-- Input: heltal R och C; sedan R rader med C tecken (’.’ = tom, ’#’ = hinder)

-- Mål: åka ut åt öster.
-- Villkor Om hon åker ut på någon annan sida av rinken (uppe, nere eller till vänster) så misslyckas hon med sitt mål.
-- Start: Natalie börjar alltid på ruta (0,0) och åker åt höger.

-- Output: det minsta antal svängar Natalie behöver göra för att ta sig ut från isen på höger sida.

import Data.Map

type Rad = Int
type Kolumn = Int
type Pos = (Rad, Kolumn)
type Hinder = Bool
type Karta = Map Pos Hinder
data Riktning = Ö | N | V | S

ettSteg :: Riktning -> Pos -> Pos
ettSteg Ö (r, k) = (r,   k+1)
ettSteg N (r, k) = (r+1, k  )
ettSteg V (r, k) = (r,   k-1)
ettSteg S (r, k) = (r-1, k  )

{-

Viktiga operationer:

åk :: Karta -> Pos -> Riktning -> Maybe Pos  -- Nothing = nothing stopping from success!

farlig :: Karta -> Pos -> Riktning -> Bool   -- kollar om en riktning är farlig (bryter mot villkoret)

-}
