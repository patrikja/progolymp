-- https://open.kattis.com/problems/towers
-- First attempt: too slow
module Main where
import Data.List (sortBy, intersperse)
import Data.Function (on)
import Test.QuickCheck

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
helper i j is js GT = compareTT (conv2 (i:is)) (conv2 (j:js))
  -- TODO: need to do some computation
  -- fallback: compare (eval (i:is)) (eval (j:js))

compareTT :: TenTower -> TenTower -> Ordering
compareTT = compare `on` fix
  where fix (n, top, is) = (n, round12 top ,is)


{-
-- example calls:
helper 2 15 [2,2] []  GT
helper 3  9 [4]   [2] GT
-}

-- simplifications:

maxVal = 100  -- TODO: figure out how high is enough
maxProd = 10^100  -- TODO: figure out how high is enough


simplify (1:is) = [] -- short for 1
simplify (i:is) = case simplify is of
  [j] | j <= maxVal && i^j <= maxProd  -> [i^j]
  js                  -> i:js
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

type TenTower = (Int, Double, [Double]) -- tower height and top exponent
  -- Invariant: 1 <= snd < 10
evalT :: TenTower -> Double
evalT (sl, top, _adj) = head (drop sl (iterate (10**) top))

conv2 :: PowerTower -> TenTower
conv2 []     = (0, 1, [])
conv2 (i:is) = pow10times l (conv2 is)
  where d = fromInteger i
        l = log10 d

round12 :: Double -> Double
round12 = round' 12

round' n f = (fromInteger $ round $ f * (10^n)) / (10.0^^n)

add :: Double -> [Double] -> [Double]
add d [] = [d]
add d [x] | x' < 2     = [x']
          | otherwise  = [d,x]
  where x' = d + x

pow10times :: Double -> TenTower -> TenTower
pow10times l (0, top, adj) = adjust (1, l*top, add l adj)
pow10times l (1, top, adj) = adjust (2, log10 l + top, add l adj)
pow10times l (2, top, adj) = adjust (3, log10 (log10 l + 10**top), add l adj)
pow10times l (n, top, adj) = (n+1, top, add l adj)

-- Make sure 1 <= result < 10
adjust :: TenTower -> TenTower
adjust (n, top, adj)
  | top < 1  = adjust (n-1, 10 ** top, adj)
  | top < 10 =        (n,         top, adj)
  | otherwise= adjust (n+1, log10 top, adj)

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

maxLen = 100
maxPow = 100

genPT :: Gen PowerTower
genPT = do
  n <- choose (1,maxLen)
  vectorOf n (choose (2,maxPow))

addOneAt :: Int -> PowerTower -> PowerTower
addOneAt 0 (p:ps) = p+1 : ps
addOneAt i (p:ps) = p : addOneAt (i-1) ps
addOneAt _ []     = []

prop1 = forAll genPT $ \pt ->
          forAll (choose (0,length pt -1)) $ \i ->
            prop1_ pt i

prop1_ pt i = i < length pt ==> comparePT pt pt' == LT
  where pt' = addOneAt i pt




eqPairs = [ ([2,2],    [4])
          , ([3,2],    [9])
          , ([11,2],  [121])
          , ([100,3], [10,6])
          , ([2,2,2],  [16])
          , ([3,2,2],  [81])
          , ([100,50], [10,100])
          ] -- TODO more equal pairs

prop2 = forAll genPT $ \pt ->
           forAll (elements eqPairs) $
             prop2_ pt

prop2_ pt (lhs, rhs) = comparePT (pt ++ lhs) (pt ++ rhs) == EQ

prop3 = forAll (listOf genPT) $ \pts -> length (solution pts) >= 0
