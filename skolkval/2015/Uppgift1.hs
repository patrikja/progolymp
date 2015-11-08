-- Kval15, Uppg. 1: Gömda ord

testcases :: [(String, String)]
testcases = [ ("ABKBFA", "ABBA")
            , ("HZBKRYAFEAAAAJ", "HEJ")
            ]

runTests sol = all (\(inp, outp) -> sol inp == outp) testcases

test = runTests solution

solution :: String -> String
solution "" = ""              -- Finns inte med i uppgiften
solution cs = head cs :       -- Första bokstaven i indata-strängen tas med i utdatasträngen.
              sol2 steg rest  -- Fortsätt framåt några steg med resten av indata-strängen
solution [c] = [c]            -- Det här fallet behöver inte vara med.
  where första = head cs
        rest   = tail cs
        steg   = char2nat första

type Nat = Int

-- | Översätt 'A' till 0, 'B' till 1, osv.
char2nat :: Char -> Nat
char2nat c = fromEnum c - fromEnum 'A'
{-
Notera att jag valde att översätta 'A' till 0, inte 1 som i uppgiften.
Men samtidigt valde jag att skicka med bara _resten_ av
indata-strängen till sol2, så det blir samma effekt.
-}

-- Hoppa fram några steg och fortsätt som från början
sol2 :: Nat -> String -> String
sol2 n cs = solution (drop n cs)
