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

-- comparePT x y = compare (conv2 x) (conv2 y)
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
helper i j is js GT = compare (conv2 (i:is)) (conv2 (j:js))
  -- TODO: need to do some computation
  -- fallback: compare (eval (i:is)) (eval (j:js))

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

-}

-- 2^100 >? 3^50
-- 100/50 = 2 >? ln 3 / ln 2 ~= 1.6

-- approximate the ratio x/y (even crudely).
-- measure size in terms of "tower height": compare to tetra 2 n ?

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

-- Normal form? This try does not work as intended.
-- 2^(x1*2^(x2*2^...) / 2^(y1*2^(y2*2^...) = 2^(x1*... - y1*...)

log10 :: Double -> Double
log10 = (/log 10) . log

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

-- ================================================================
-- Next try for a "normal form":
--   rewrite each tower into 10^10^...^x
--   with 1<x<10
-- Then comparison by lexicographical ordering of pairs
--   (when the accuracy is good enough).

type TenTower = (Int, Double) -- tower height and top exponent
  -- Invariant: 1 <= snd < 10
evalT :: TenTower -> Double
evalT (sl, top) = head (drop sl (iterate (10**) top))

conv2 :: PowerTower -> TenTower
conv2 []     = (0, 1)
conv2 (i:is) = conv2' l sl top
  where d = fromInteger i
        l = log10 d
        (sl, top) = conv2 is

conv2' :: Double -> Int -> Double -> TenTower
conv2' l 0 top = adjust (1, l*top)
conv2' l 1 top = adjust (2, log10 l + top)
conv2' l 2 top = adjust (3, log10 (log10 l + 10**top))
conv2' l n top = (n+1, top) -- approximately

-- Assume 0.1 < top < 10^10
-- Make sure the 1 <= result < 10
adjust :: TenTower -> TenTower
adjust (n, top) | top < 1  = (n-1, 10 ** top)
                | top < 10 = (n,         top)
                | otherwise= (n+1, log10 top)

{-
TODO: check more carefully if conv2 gives very similar results for two
arguments. (May be accidentally ordered in the wrong way.)

TODO: Perhaps get slightly better precision with smaller base (like 2)
instead of 10.
-}

{- Solve for tt: i ^ is == evalT tt
   without actually computing i^is (unless it is small).

  i ^ is == evalT tt
=
  10^(log10 i * is) == evalT tt
= let l = log10 i
  10^(l * evalT (sl, top)) == evalT (sl', top')
-- two cases: sl == 0 or > 0
  10^(l * top) == evalT (sl', top')
  -- three new cases: l*top <1, or <10 or >=10
  -- the first two cases give
    sl' = 0 and top' = l*top
  -- and the third case gives
    sl' = 1 and top' = log10 (l*top)
-- second case: sl = slm + 1
  10^(l * 10^evalT (slm, top)) == evalT (sl', top')
-- let sl' = 1 + slm'
  10^(l * 10^evalT (slm, top)) == 10 ^ evalT (slm', top')
-- simplify
  l * 10^evalT (slm, top) == evalT (slm', top')
-- case: slm == 0
    l * 10^top == evalT (slm', top')
  -- simple renormalisation in two cases:
    -- x=l*10^top < 10
      slm' = 0  and  top' = x
    -- 10 <= x < 10^10
      slm' = 1  and  top' = log10 x
-- case: slm == 1
  -- We need to solve:
    l * 10^(10^top) == 10^evalT (slm'-1, top')
  -- take log10
    log10 l + 10^top == evalT (slm'-1, top')
  -- ll = log10 l = log10 (log10 i) is between -0.53 and 0.31
  -- so 1<=top<10 will change very little to become top'
    -- case top' < 1: set top = 10^top instead and adjust slm'
    -- case top' > 1: (almost always): simple
-- case slm > 1
  -- now the correction would be negligible: a change of < 3e-11
  thus we can approximate top' = top
  (good, because 10^10^x is mostly out of the range of Double)
    (max double is 1.79769e+308 = 10^10^2.49 = (2, 2.49) in our notation)

-}


exampleInputs =
  [ "2^2^3^2^2^2^2^2^2^2^2"
  , "3^2^2^2^2^2^2^2^2^2^2"
  , "2^2^2^2^3^2^2^2^2^2^2"
  , "2^2^2^2^2^2^2^2^2^2^2"
  , "3^3^3^3^3^3^3^3"
  , "4^4^4^4^4"
  , "5^5^5^5"
  , "6^6^6^6"
  , "7^7^7"
  , "8^8^8"
  , "100^100"
  ]

test  = map (conv . line2PowerTower) exampleInputs
test2 = map (conv2 . line2PowerTower) exampleInputs
test3 = sortBy comparePT (map line2PowerTower exampleInputs)
