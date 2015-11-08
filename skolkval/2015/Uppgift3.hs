testcases = [ (1, (0, 1))
            , (4, (1, 3))
            , (8, (3, 5))
            , (13,(4, 9))
            ]

-- Alf och Beata
solution :: Int -> (Int, Int)
solution 0 = (0, 0)
solution n | n > 0 = (alf, beata + mer)
  where mindre  = n `div` 2
        mer     = n - mindre
        (beata, alf) = solution mindre

runTests sol = all (\(inp, outp) -> sol inp == outp) testcases
test = runTests solution

main = do
  putStr "Antal muffins ? "
  n <- readLn
  let (alf, beata) = solution n
  putStrLn $ "Svar: " ++ show alf ++ " " ++ show beata
