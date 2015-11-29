-- Skridskor
-- Problem ID: skridskor Time limit: 1 second Memory limit: 1024 MB

-- Input: heltal R och C; sedan R rader med C tecken (’.’ = tom, ’#’ = hinder)

-- Mål: åka ut åt öster.
-- Villkor Om hon åker ut på någon annan sida av rinken (uppe, nere eller till vänster) så misslyckas hon med sitt mål.
-- Start: Natalie börjar alltid på ruta (0,0) och åker åt höger.

-- Output: det minsta antal svängar Natalie behöver göra för att ta sig ut från isen på höger sida.

import Data.Array
import BFS

type Rad      = Int
type Kolumn   = Int
type Pos      = (Rad, Kolumn)
type Hinder   = Bool
type Karta    = (Pos, Array Pos Hinder)
data Riktning = Ö | N | V | S
  deriving (Eq, Ord, Show)

ettSteg :: Riktning -> Pos -> Pos
ettSteg Ö (r, k) = (r,   k+1)
ettSteg N (r, k) = (r+1, k  )
ettSteg V (r, k) = (r,   k-1)
ettSteg S (r, k) = (r-1, k  )

type RPos = (Riktning, Pos)
type UPos = Either Riktning RPos -- Left r betyder utanför kartan i riktning r

start :: UPos
start = Right (Ö, (0, 0))

byggSökproblem :: Karta -> Sökproblem UPos
byggSökproblem karta = Sökproblem nästa vinst
  where
    -- Vi har en funktion som anger alla möjliga nästa positioner givet en startposition:
    nästa :: UPos -> [UPos]
    nästa (Left r)    = [] -- inga fler steg från utanför kartan
    nästa (Right pos) = case gå karta pos of
      Left r          -> [Left r]
      Right (rikt, p) -> [Right (vänster rikt, p),
                          Right (höger   rikt, p)]

    -- vi har en funktion som testar om en position är "vinnande"
    vinst :: UPos -> Bool
    vinst (Left Ö) = True
    vinst _        = False

gå :: Karta -> RPos -> UPos
gå karta (rikt, pos) | utanför karta nästa = Left rikt
                     | hinder  karta nästa = Right (rikt, pos)
                     | otherwise           = gå karta (rikt, nästa)
  where nästa = ettSteg rikt pos

utanför :: Karta -> Pos -> Bool
utanför ((maxrad, maxkol), _) (r, k) =   r < 0 || r >= maxrad
                                     ||  k < 0 || k >= maxkol
hinder :: Karta -> Pos -> Bool
hinder (_, karta) p = karta!p

höger :: Riktning -> Riktning
höger Ö = S
höger N = Ö
höger V = N
höger S = V

vänster :: Riktning -> Riktning
vänster S = Ö
vänster Ö = N
vänster N = V
vänster V = S

-- Nu har vi egentligen beskrivit en graf: noder är (utökade)
-- positioner och närmaste grannar anges av funktionen nästa.
-- Uppgiften blir att hitta kortaste vägen i grafen från en startnod
-- till en vinst. Det problemet kan lösas med "bredden-först"-sökning
-- som håller reda på vilka noder som redan besökts.

körSökproblem :: Sökproblem UPos -> Int
körSökproblem sp = lösning sp start   - 1

solution :: Karta -> Int
solution = körSökproblem . byggSökproblem

----------------------------------------------------------------

byggKarta :: (Pos, [[Char]]) -> Karta
byggKarta ((maxr, maxk), text) =
          ((maxr, maxk), listArray ((0,0), (maxr-1, maxk-1))
                                   (map teckenTillHinder (concat text)))

teckenTillHinder :: Char -> Hinder
teckenTillHinder = ('#'==)

main = do
  rOchK <- getLine
  let rader, kolumner :: Int
      (rader, rest):_ = reads rOchK
      (kolumner, _):_ = reads rest
  rows <- sequence (replicate rader (take kolumner `fmap` getLine))
  let karta = byggKarta ((rader,kolumner),rows)
  let svar  = solution karta
  print svar
-- för felsökning:
--  let sp    = byggSökproblem karta
--  let lista = bfs sp start
--  print (take 50 lista)
