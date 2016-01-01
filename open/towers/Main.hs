-- https://open.kattis.com/problems/towers
module Main where
import Data.List (sortBy, intersperse, elemIndex)
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

-- TODO: improve this "ad-hoc" list
add :: Double -> [Double] -> [Double]
add d [] = [d]
add d [x] | x' <= 100 = [x']
  where x' = 10**x * d
add d xs = d:xs
-- assume d = log10 a
--    and x = log10 b
-- we need log10 (a^b) = b*log10 a = b*d = 10**x*d


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
  vectorOf n (choose (1,maxPow))

addOneAt :: Int -> PowerTower -> PowerTower
addOneAt 0 (p:ps) = p+1 : ps
addOneAt i (p:ps) = p : addOneAt (i-1) ps
addOneAt _ []     = []

prop1 = forAll genPT $ \pt ->
          forAll (choose (0,length pt -1)) $ \i ->
            prop1_ pt i

prop1_ pt i = (cmp == LT) ||
              (cmp == EQ && maybe (-1) id idx < i)
  where pt' = addOneAt i pt
        cmp = comparePT pt pt'
        idx = elemIndex 1 pt -- anything after a 1 changes nothing

eqPairs = [ ([2,2],    [4])
          , ([3,2],    [9])
          , ([11,2],  [121])
          , ([100,3], [10,6])
          , ([2,2,2],  [16])
          , ([3,2,2],  [81])
          , ([100,50], [10,100])
          , ([49,49],  [7,98])
          , ([2,8,3],  [4,16,2])
          , ([2,8,7],  [4,16,5])
          ] -- TODO more equal pairs

prop2 = forAllShrink genPT shPT $ \pt ->
           forAll (elements eqPairs) $
             prop2_ pt

prop2_ pt (lhs, rhs) = comparePT (pt ++ lhs) (pt ++ rhs) == EQ

-- Just check for crashes
prop3 = forAll (listOf genPT) $ \pts -> length (solution pts) >= 0

----------------------------------------------------------------
{-

Exploring exact equality:

When is m^x == n^y with 1<=m,n<=100 ?

Case on m:
  m == 1: then m^x == 1 for all x and n must be 1 (and y arbitrary).

Otherwise, if m == n then x == y is the only solution.

Otherwise m = product mps where mps is an ordered, nonempty list of primes
and       n = product nps -- " --

or equivalently m = prodexp mpes with

-}
type Prime  = Integer
type Mult   = Integer
type Factors = [(Prime, Mult)]
prodexp :: Factors -> Integer
prodexp = foldr (\(p,e) a -> p^e*a) 1

{-

We can represent any positive integer by a unique such list of pairs -
there is an inverse

factorise :: Integer -> Factors
-- See https://hackage.haskell.org/package/arithmoi

The "power" operation is eaily performed on the Factors: just multiply
the multiplicities.

-}

power :: Factors -> Integer -> Factors
power fs e = map (mapFst (e*)) fs

mapFst :: (a->b) -> (a,c) -> (b,c)
mapFst f (a, c) = (f a, c)

{-

Thanks to uniqueness of the prime factorisation, the equation

  power mpes x == power npes y

reduces to

  forall (p,k) in mpes there must be (p,l) in npes with
    x*k == y*l

This means that
* exactly the same prime factors appear in n and m
* their multiplicites have to satisfy x*k == y*l

Example: 2^x /= 3^y for any x,y>0

Now we can start to classify solutions:
  mpes = [(2,k)]  -- for k in [1..6] covers [2,4,8,16,32,64]
then
  npes = [(2,l)]
are the only possible solutions (and we have taken care of k==l earlier).

But we also need x*k == y*l.
  For example k==1, l==2 means that x == 2*y.

  We can equate the mult. of the prime factors of x and 2*y:
    mult 2 x = 1 + mult 2 y
  and for all p>2
    mult p x = mult p y.
  Now if we use that x=b^v and y=c^w we get
    v*mult 2 b = 1 + w*mult 2 c
  and for all p>2
    v*mult p b = w*mult p c.

  The second eq. means that (as above) the same prime factors (>2)
  have to appear in both b and c. And if some actually appear it
  restricts v and w severely.

    In case none (>2) appear we have b=2^bm and c=2^cm and v*bm = 1 +
    w*cm.

      For example for bm=3 and cm=4 we get v*3 = 1 + w*4 with
      solutions v=3+4q, w=2+3q for q >= 0.  To summarise, this example
      is m=2, n=4, x=8^(3+4q), y=16^(2+3q). (So eval [2,8,3] == eval
      [4,16,2]. But also [2,8,7] ~= [4,16,5] osv.)

    If 3 appears as a factor, say once in b and in c, then v==w which
    requires v*bm = 1 + v*cm which means v*(bm-cm)==1 so that v==w==1
    and bm=cm+1. So we have eval [2,2*2^cm*3,1] == eval [4,2^cm*3,1].
     eval [2,6,1] == eval [4,3,1]

Many other special cases can be found by digging further, but the
question is: how do we compute the equality check?

-}

shPT [] = []
shPT xs = [init xs]

-- Trying to find new equal pair by "provoking quickCheck"
prop4 = forAllShrink (vectorOf 5 (choose (1,100))) shPT $ \is ->
        forAllShrink (vectorOf 5 (choose (1,100))) shPT $ \js ->
        simplify is /= simplify js ==> comparePT is js /= EQ

test4 =  quickCheckWith stdArgs { maxSuccess = 5000 } prop4

----------------------------------------------------------------
{-

TODO: develop better comparison test to catch these cases:

λ> quickCheck prop2
*** Failed! Falsifiable (after 1 test and 8 shrinks):
[]
([2,8,3],[4,16,2])
λ> quickCheck prop2
*** Failed! Falsifiable (after 14 tests and 8 shrinks):
[]
([2,8,7],[4,16,5])

-}
