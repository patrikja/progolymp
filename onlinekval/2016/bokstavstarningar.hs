import Data.List
{-
Klara har N bokstavstärningar. Varje tärning har en bokstav på var och en av sina 2≤K≤20 sidor. Genom att slå tärningarna och ordna dem på valfritt sätt kan man skapa ett ord med N bokstäver.

Skriv ett program som räknar ut hur många giltiga ord man kan skriva med Klaras bokstavstärningar. Till din hjälp har du en ordlista som innehåller alla M giltiga ord med N bokstäver.

Input:

Den första raden i indatan innehåller tre heltal: N, K och M. Därefter följer N rader med vardera K bokstäver, bokstäverna på varje tärning. Sist kommer M rader med vardera N bokstäver, listan på alla giltiga ord.
Enbart stora engelska bokstäver (A-Z) förekommer i indata.
Ingen bokstav kan förekomma på mer än en sida av en tärning.

Output
Ditt program ska skriva ut ett enda heltal: antalet giltiga ord som kan skrivas.

Analys:
För stora N och K finns det på tok för många utfall för att "generera och testa" ska fungera. Bättre att gå igenom ordlistan istället och kolla vilka ord som kan genereras.

För det behöver vi en funktion som testar om ett givet ord (från ordlistan) kan genereras av en uppsättning tärningar.

Ett delproblem är att för en bokstav finna de tärningar den kan genereras från.

(Vi kan representera en tärning med dess nummer från 1..N.)
-}
type Tärning = Int

-- kanGenBokstav :: Char -> [Tärning]
-- kanGenBokstav = error "TODO"

{-
Sedan kör vi den för alla bokstäver i ordet
-}

-- kanGenOrd :: String -> [[Tärning]]
-- kanGenOrd = map kanGenBokstav

{-

Nu har vi en lista av N mängder av tal inom [1..N] och vi vill avgöra
om det går att hitta alla tal från 1 till N genom att ta exakt ett tal
från varje mängd. Det är lämpligt att börja med små mängder, så vi kan
sortera listan med avseende på ingående listans längd.
-}

kortaFörst :: [[a]]->[[a]]
kortaFörst = sortBy (\xs ys -> compare (length xs) (length ys))

{-

Om någon dellista är tom är ordet omöjligt att generera, om någon har
bara ett element måste vi välja det (och kan ta bort det från resten
av mängderna). [Kanske sortera igen för att snabbt hitta nästa
"minsta" mängd.] Om det finns mer än ett alternativ måste vi testa
alla (men det räcker att hitta en lösning).

-}

testa :: [[Tärning]] -> [Tärning] -> Bool
testa [] [] = True
testa [] _  = False
-- testa (ts:tss) []  -- ska inte ske
testa (ts:tss) kvar = any f ts
  where f t = t `elem` kvar && testa tss (delete t kvar)

-- Från uppgiftstexten: tärningarna för ordet "KATT"
t1 :: [[Tärning]]
t1 = [[2,3], [4], [1,3], [1,3]]

main = do
  tal <- getLine
  let n, k, m :: Int
      [n, k, m] = map read (words tal)
  tärningar <- sequence (replicate n getLine)
  ordlista  <- sequence (replicate m getLine)
  print (length (filter id (solution n (tkn tärningar) (tkn ordlista))))

tkn :: [String] -> [[TärningsSida]]
tkn = map (map (\c->fromEnum c - fromEnum 'A'))

type Antal = Int
type Tecken = Int  -- A=0, B=1, ... upp till maximalt Z
type TärningsSida = Tecken
type Ordet = [TärningsSida]

solution :: Int -> [[TärningsSida]] -> [Ordet] -> [Bool]
solution n tss ords = map kollaEttOrd ords
  where kanGenBokstav :: Tecken -> [Tärning]  -- TODO gör till Array
        kanGenBokstav t = [i | (i, ts) <- numrera tss, t `elem` ts]
        kanGenOrd :: [Tecken] -> [[Tärning]]
        kanGenOrd = map kanGenBokstav
        kollaEttOrd ts = testa qs [1..n]
          where qs = kortaFörst (kanGenOrd ts)


numrera = zip [1..]
