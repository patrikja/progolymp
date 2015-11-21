{-

mobil: 100 ms delay -> laptop reagerar.
uppdrag: hur många ms som laptopen spelade upp musik
Innan första kommandot utförs så är spelaren i pausat läge.

Input: Den första raden innehåller heltalet 1≤N<1000, antalet loggrader.
tidsstämpel i millisekunder, enhet (laptop eller mobile), kommando (play eller paus).
Output: antalet millisekunder användaren lyssnat på musik

4
0000500 laptop play
0001000 laptop paus
0001500 mobile play
0002000 laptop paus
900

4
0000001 laptop play
0000004 mobile play
0000100 mobile paus
0000103 laptop paus
198

-}

import Lib

solution :: [Cmd] -> Tid
solution = sumUp . order

sumUp :: [Cmd] -> Tid
sumUp cmds | null rest1  = 0
           | null rest2  = error "Fel: loggen måste sluta med Paus"
           | otherwise   = t2-t1 + sumUp (tail rest2)
  where rest1          = dropWhile isPaus cmds
        (t1,(_,True))  = head rest1
        rest2          = dropWhile isPlay rest1
        (t2,(_,False)) = head rest2


isPaus :: Cmd -> Bool
isPaus = not . isPlay
isPlay :: Cmd -> Bool
isPlay (_,(_,p)) = p

order :: [Cmd] -> [Cmd]
order cmds = mergeBy fst laptop (map (mapFst (100+)) mobile)
  where laptop = filter (\(_,(m,_)) -> not m) cmds
        mobile = filter (\(_,(m,_)) -> m)     cmds

mergeBy :: Ord b => (a->b) -> [a] -> [a] -> [a]
mergeBy f [] ys = ys
mergeBy f xs [] = xs
mergeBy f (x:xs) (y:ys) | f x <= f y = x : mergeBy f xs (y:ys)
                        | otherwise  = y : mergeBy f (x:xs) ys

mapFst :: (a->b) -> (a,c) -> (b,c)
mapFst f (a, c) = (f a, c)


type Tid = Int
type Play = Bool
type Delayed = Bool
type Cmd = (Tid, (Delayed, Play))
parseCmd :: String -> Cmd
parseCmd raw = (read field1, ("mobile"==field2, "play"==field3))
  where [field1, field2, field3] = words raw

main = do
  n <- readLn
  rawcmds <- sequence (replicate n getLine)
  let cmds = map parseCmd rawcmds
--  print (cmds, solution cmds) -- for debugging
  print (solution cmds)
