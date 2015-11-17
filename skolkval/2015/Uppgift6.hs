-- Uppgift 6 – Tankeläsning
import Data.Ratio
import Lib

testcases =
  [ (["- 1", "* 3", "+ 9", "/ 3", "+ 5", "- x"], "7")
  , (["- 1", "* 3", "+ 9", "/ 2", "+ 5", "- x"], "Nej")
  , (["+ 2", "- x", "* x", "/ 2", "+ 3", "- x"], "3")
  , (["+ 7", "* x", "* 0", "* x", "- 7"],        "-7")
  , (["* x"],                                    "Nej")
  , (["* 3", "/ 3", "- x", "+ 5"],               "5")
  , (["+ 1", "- x", "/ 2"],                      "Nej")
  ]

-- Eftersom "/ x" inte tillåts kommer alla uttryck att vara polynom.
-- Ett polynom kan representeras med en lista av koefficienter som
-- börjar med den konstanta termen.  Exempelvis 3x+2 blir P [2,3]
newtype Polynom = P {unP :: [Rational]}
  deriving Show

startP :: Polynom
startP = P [0,1]

type Tecken = Char
type Siffra = Rational
data KanskeSiffra = S Siffra | X
type Steg   = (Tecken, KanskeSiffra)

uppdatera :: Polynom -> Steg -> Polynom
uppdatera (P (k:ks))    ('+', S s) = P (k + s   : ks)
uppdatera (P (k:ks))    ('-', S s) = P (k - s   : ks)
uppdatera (P ks    )    ('*', S s) = P (map (* s) ks)
uppdatera (P ks    )    ('/', S s) = P (map (/ s) ks)
uppdatera (P (0:ks))    ('/', X  ) = P ks  -- vi kan hantera vissa divisioner med X
uppdatera (P (_:ks))    ('/', X  ) = error "Inte tillåtet att dividera med X"
uppdatera (P ks    )    ('*', X  ) = P (0:ks)
uppdatera (P (k0:k1:ks))('+', X  ) = P (k0 : k1+1 : ks)
uppdatera (P (k0:k1:ks))('-', X  ) = P (k0 : k1-1 : ks)

fleraSteg :: Polynom -> [Steg] -> Polynom
fleraSteg p []     = p
fleraSteg p (s:ss) = fleraSteg p1 ss
  where p1 = uppdatera p s

kolla :: Polynom -> Maybe Integer
kolla (P (k:ks))
  | heltal k && all (0==) ks = Just (numerator k)
kolla _ = Nothing

heltal :: Rational -> Bool
heltal r = denominator r == 1

type InSteg = String -- tecken, mellanslag, siffra eller x

läsSteg :: InSteg -> Steg
läsSteg (t : ' ' : 'x' : "")    = (t, X)
läsSteg (t : ' ' : c   : "")
  | '0' <= c && c <= '9'  = (t, S (fromIntegral (fromEnum c - fromEnum '0')))
läsSteg s                 = error ("Otillåten indata: "++s)

fleraInSteg :: [InSteg] -> Polynom
fleraInSteg = fleraSteg startP . map läsSteg

solution :: [InSteg] -> String
solution xs = case kolla (fleraInSteg xs) of
  Nothing -> "Nej"
  Just s  -> show s

runTests sol = all (\(inp, outp) -> sol inp == outp) testcases
test = runTests solution

main = do
  putStr "Antal operationer ? "
  n <- readLn
  ins <- mapM (\_ -> putStr "Operation ? " >> getLine) [1..n]
  svar (solution ins)
