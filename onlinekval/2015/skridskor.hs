-- Skridskor
-- Problem ID: skridskor Time limit: 1 second Memory limit: 1024 MB

-- Input: heltal R och C; sedan R rader med C tecken (’.’ = tom, ’#’ = hinder)

-- Mål: åka ut åt öster.
-- Villkor Om hon åker ut på någon annan sida av rinken (uppe, nere eller till vänster) så misslyckas hon med sitt mål.
-- Start: Natalie börjar alltid på ruta (0,0) och åker åt höger.

-- Output: det minsta antal svängar Natalie behöver göra för att ta sig ut från isen på höger sida.

import Data.Array

type Rad = Int
type Kolumn = Int
type Pos = (Rad, Kolumn)
type Hinder = Bool
type Karta = (Pos, Array Pos Hinder)
data Riktning = Ö | N | V | S
  deriving (Eq, Show)

ettSteg :: Riktning -> Pos -> Pos
ettSteg Ö (r, k) = (r,   k+1)
ettSteg N (r, k) = (r+1, k  )
ettSteg V (r, k) = (r,   k-1)
ettSteg S (r, k) = (r-1, k  )

-- Viktiga operationer:

data UtökadPos = Bra Pos | Klar | Farlig
  deriving (Eq, Show)

-- kollar om en utökad position är farlig (bryter mot villkoret)
farlig :: UtökadPos -> Bool
farlig = (Farlig==)

åk :: Karta -> Riktning -> Pos -> UtökadPos
åk karta rikt pos | utanför karta nästa  = if rikt==Ö then Klar else Farlig
                  | hinder  karta nästa  = Bra pos
                  | otherwise            = åk karta rikt nästa
  where nästa = ettSteg rikt pos

utanför :: Karta -> Pos -> Bool
utanför ((maxrad, maxkol), _) (r, k) =   r < 0 || r >= maxrad
                                     ||  k < 0 || k >= maxkol

hinder :: Karta -> Pos -> Bool
hinder (_, karta) p = karta!p

-- sök :: Karta -> Pos -> Rikt -> UtökadPos
sök :: Karta -> Riktning -> Pos -> Tree Pos
sök karta rikt pos = case åk karta rikt pos of
  Farlig -> Fail
  Klar   -> Done pos
  Bra p  -> Try  pos (sök karta (vänster rikt) p)
                     (sök karta (höger   rikt) p)

data Tree a = Fail | Done a | Try a (Tree a) (Tree a)
  deriving Show
-- Exempel:
t1 :: Tree Int
t1 = Try 1 (Try 2 (Done 3) Fail) (Try 4 (Try 5 (Done 6) (Done 7)) (Done 8))


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
  rows <- sequence (replicate rader getLine)
  let karta = byggKarta ((rader,kolumner),rows)
  let träd = sök karta Ö (0,0)
  let svar = djupFörFörstaLösningen träd
  print svar
  -- print (takeTree (svar+2) träd) -- for debugging

-- | Sök igenom trädet en "nivå" i taget
bräddenFörst :: Tree a -> [[a]]
bräddenFörst Fail        = []  : repeat []
bräddenFörst (Done x)    = [x] : repeat []
bräddenFörst (Try x l r) = []  : zipWith (++) lss rss
  where lss = bräddenFörst l
        rss = bräddenFörst r

numrera :: (Num n, Enum n) => [a] -> [(n, a)]
numrera = zip [0..]

djupFörFörstaLösningen :: Eq a => Tree a -> Int
djupFörFörstaLösningen = fst . head . dropWhile (null.snd) . numrera . bräddenFörst
-- djupFörFörstaLösningen = fst . head . dropWhile (null.snd) . numrera . bräddenFörst' []

-- Ett försöka att spara tid genom att undvika "loopar"
bräddenFörst' :: Eq a => [a] -> Tree a -> [[a]]
bräddenFörst' xs Fail        = []  : repeat []
bräddenFörst' xs (Done x)    = [x] : repeat []
bräddenFörst' xs (Try x l r) | x `elem` xs = []  : repeat [] -- Loop
                             | otherwise   = []  : zipWith (++) lss rss
  where lss = bräddenFörst' (x:xs) l
        rss = bräddenFörst' (x:xs) r


takeTree 0 t = Fail
takeTree n Fail = Fail
takeTree n (Done x) = Done x
takeTree n (Try x l r) = Try x (takeTree (n-1) l)
                               (takeTree (n-1) r)


-- Alternativ typ
-- import qualified Data.Map as Map
-- import Data.Map (Map)
-- type Karta = (Pos, Map Pos Hinder)
-- hinder (_, karta) p = Map.lookup p karta == Just True
