-- Skridskor
-- Problem ID: skridskor Time limit: 1 second Memory limit: 1024 MB

-- Input: heltal R och C; sedan R rader med C tecken (’.’ = tom, ’#’ = hinder)

-- Mål: åka ut åt öster.
-- Villkor Om hon åker ut på någon annan sida av rinken (uppe, nere eller till vänster) så misslyckas hon med sitt mål.
-- Start: Natalie börjar alltid på ruta (0,0) och åker åt höger.

-- Output: det minsta antal svängar Natalie behöver göra för att ta sig ut från isen på höger sida.

type Rad = Int
type Kolumn = Int
type Pos = (Rad, Kolumn)
type Hinder = Bool
type Karta = Map Pos Hinder
{-

Viktiga operation:

åk :: Karta -> Pos -> Riktning -> Maybe Pos  -- Nothing = nothing stoppnig from success!



-}
