-- Kval15, Uppg. 2: Tabbtabbande
import Lib

testcases :: [((Int, [Int]), Int)]
testcases = [ ((5, [2, 5, 4]),        4)
            , ((9, [5, 9, 4, 9, 8]), 17)
            ]

-- | Beräkna kortaste avståndet mellan i och j "modulo n". Exempel: t 5 (1, 2) == 1 == t 5 (1, 5)
tabbavstånd :: Int -> (Int, Int) -> Int
tabbavstånd n (i, j) = minimum $ map (`mod` n) $ [i-j, j-i]

solution :: (Int, [Int]) -> Int
solution (n, ps) = sum $                   -- Räkna ut summan av
                   map (tabbavstånd n) $   -- alla minsta tabbavstånd
                   zip (1:ps) ps           -- för alla närliggande par av tabb-nummer.



runTests sol = all (\(inp, outp) -> sol inp == outp) testcases
test = runTests solution

main = do
  putStr "Antal tabbar ? "
  n <- readLn
  ts <- getList  "Antal växlingar ? "
                 "Växla till ? "
  putStrLn $ "Svar: " ++ show (solution (n, ts))
