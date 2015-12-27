-- https://open.kattis.com/problems/towers
-- First attempt: too slow
module Main where
import Data.List (sortBy, intersperse)
import Data.Function (on)

type PowerTower = [Integer]

main = do
  n <- readLn
  lines <- sequence (replicate n getLine)
  let pts = map line2PowerTower lines
  putStr (solution pts)

solution :: [PowerTower] -> String
solution = unlines . ("Case 1:":) . map showPT . sortBy comparePT

{-
-- This helper function is in Data.List now but was not in the version used by Kattis (ghc-7.6.3)
sortOn :: Ord b => (a->b) -> [a] -> [a]
sortOn f = sortBy (compare `on` f)
-}

showPT :: PowerTower -> String
showPT = concat . intersperse "^" . map show

line2PowerTower :: String -> PowerTower
line2PowerTower s = is
  where is = map read (splitBy '^' s)

splitBy :: Eq a => a -> [a] -> [[a]]
splitBy sep xs | null first = []
               | otherwise  = first : splitBy sep (drop 1 rest)
  where
    (first, rest) = span (sep/=) xs

eval :: [Integer] -> Integer
eval = foldr (^) 1

{-

It is too expensive to fully compute the integers corresponding to the
"PowerTowers":
p 2 1 = 2   = 2
p 2 2 = 2^2 = 4
p 2 3 = 2^4 = 16
p 2 4 = 2^16= 65536
p 2 5 = 2^65536 = (a 1 followed by 65536 zeros in binary)
p 2 6 = 2^(p 5) = already ridiculously large!
and the task allows for numbers up to a maximum of
p 100 100

Thus the main question is how to efficiently compare two numbers given
in the "PowerTower" format.

-}

comparePT x y = compPT (simplify x) (simplify y)

-- Base cases (assuming the "1-simplification" has been done)
compPT (i:is)  []      = GT
compPT []      (j:js)  = LT
compPT []      []      = EQ
compPT (i:is) (j:js) = case compare i j of
  EQ -> compPT is js -- a nice optimization
  LT -> helper i j is js (compPT is js)
  GT -> flipOrd (helper j i js is (compPT js is))
     -- flipOrd (compPT (j:js) (i:is))

flipOrd LT = GT
flipOrd GT = LT
flipOrd EQ = EQ

-- precondition: helper i j is only called with i < j
helper i j is js LT = LT
helper i j is js EQ = LT
helper i j is js GT = compare (eval (i:is)) (eval (j:js))
  -- TODO: need to do some computation

{-
-- example calls:
helper 2 15 [2,2] []  GT
helper 3  9 [4]   [2] GT
-}

-- simplifications:

simplify (1:is) = [] -- short for 1
simplify (p:is) = p : simplify is
simplify [] = []

----------------

{-
Left to do: compute helper i j is js GT in a more clever way

Facts: eval is >= 1 + eval js

  i^x >? j^y
<=>
  i^x / j^y >? 1
<=>
  ln (i^x / j^y) = x*ln i - y*ln j >? 0
<=>
  x/y >? ln j/ln i

ln j/ln i is never more than 7 but x/y is often huge

so it may be useful to approximate the ratio x/y (even crudely).

----------------

(2^x)^y = 2^(x*y)

x1 = 2log i
y1 = 2log j

2^(x1*2^(x2*2^(x3*...*2))) / 2^(y1*2^(y2*2^(y3*...*2))) =
2^(x1*2^(x2*2^(x3*...*2)) - y1*2^(y2*2^(y3*...*2))) =

2^(2log x1 + x2*2^(x3*...*2)) - 2^(2log y1 + y2*2^(y3*...*2))


-}

-- 2^100 >? 3^50
-- 100/50 = 2 >? ln 3 / ln 2 ~= 1.6

-- approximate the ratio x/y (even crudely).
-- measure size in terms of "tower height": compare to tetra 2 n

height :: PowerTower -> Integer
height []     = 0
height [i]    = slog2 i
height [i,j]  = slog2 (i^j)
height (2:is) = 1 + height is
height (i:is) = 1 + height is -- approximately

-- integer approximation of "super-log"
slog2 :: Integer -> Integer
slog2 1 = 0
slog2 2 = 1
slog2 4 = 2
slog2 16 = 3
slog2 65536 = 4
slog2 n = 1 + slog2 (log2 n)

-- Only defined for positive integers
log2 :: Integer -> Integer
log2 1 = 0
log2 2 = 1
log2 3 = 1
log2 4 = 2
log2 n = 1 + log2 (div n 2)

----------------------------------------------------------------

-- Normal form? Does not work as intended.
-- 2^(x1*2^(x2*2^...) / 2^(y1*2^(y2*2^...) = 2^(x1*... - y1*...)

log10D :: Integer -> Double
log10D = (/log 10) . log . fromInteger

-- 2^2^2^2 / 15^2^2


-- One option:
-- small = not . isInfinite

small = (<10^10)

-- Combine the tail into a Double (as big as can fit)
conv :: PowerTower -> [Double]
conv [] = []
conv (i:is) = if length es == 1  && small de then
                [de]
              else
                d:es
  where de = d**e
        d = fromInteger i
        e = head es
        es = conv is

-- then also try to rewrite into 10^10^...^x
--   with 1<x<10
-- i < 10

exampleInputs =
  [ "2^2^2^2^2^2^2^2^2^2^2"
  , "3^3^3^3^3^3^3^3"
  , "4^4^4^4^4"
  , "5^5^5^5"
  , "6^6^6^6"
  , "7^7^7"
  , "8^8^8"
  , "100^100"
  ]

test = map (conv . line2PowerTower) exampleInputs
