-- https://open.kattis.com/problems/towers
module Main where
import Data.List (sortBy, intersperse, elemIndex, sort, groupBy)
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
compPT is      js      | eqPT is js
                       = EQ
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

-- log2 rounded up
log2up :: Integer -> Integer
log2up 1 = 0
log2up n | even n     = 1 + log2up (    div n 2)
         | otherwise  = 1 + log2up (1 + div n 2)

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
          , ([2,8,99], [4,16,74])
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
shPT xs = init xs :
             map (:tail xs)  (shPos (head xs))
          ++ map (head xs :) (shPT (tail xs))

shPos n = map (1+) (shrink (n-1))


-- Trying to find new equal pair by "provoking quickCheck"
prop4 = forAllShrink (vectorOf 5 (choose (1,100))) shPT $ \is ->
        forAllShrink (vectorOf 5 (choose (1,100))) shPT $ \js ->
        simplify is /= simplify js ==> comparePT is js /= EQ

test4 =  quickCheckWith stdArgs { maxSuccess = 5000 } prop4

testeqPT = quickCheck (\(Positive n) (Positive m) -> (n <= 10) ==> (m <= 50) ==> eqPT [n,2*m] [n^2,m])

prop5 = forAllShrink genPT shPT $ \is ->
          forAllShrink genPT shPT $ \js ->
            prop5_ is js
prop5_ is js = eqPT is js == (comparePT is js == EQ)
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
-- start of borrowing from https://wiki.haskell.org/Prime_numbers
_Y   :: (t -> t) -> t
_Y g = g (_Y g)                -- multistage, non-sharing, recursive
       -- g x where x = g x    -- two stages, sharing, corecursive
primesW = [2,3,5,7] ++ _Y ( (11:) . tail  . gapsW 11 wheel . joinT .
                            map (\p->
                              map (p*) . dropWhile (< p) $
                                scanl (+) (p - rem (p-11) 210) wheel) )

gapsW k (d:w) s@(c:cs) | k < c     = k : gapsW (k+d) w s    -- set difference
                       | otherwise =     gapsW (k+d) w cs   --   k==c
hitsW k (d:w) s@(p:ps) | k < p     =     hitsW (k+d) w s    -- intersection
                       | otherwise = scanl (\c d->c+p*d) (p*p) (d:w)
                                       : hitsW (k+d) w ps   --   k==p
wheel = 2:4:2:4:6:2:6:4:2:4:6:6:2:6:4:2:6:4:6:8:4:2:4:2:
        4:8:6:4:6:2:4:6:2:6:6:4:2:4:6:2:6:4:2:4:2:10:2:10:wheel
-- joinL ((x:xs):t) = x : union xs (joinL t)
joinT ((x:xs):t) = x : union xs (joinT (pairs t))    -- set union, ~=
  where  pairs (xs:ys:t) = union xs ys : pairs t     --    nub.sort.concat

union (x:xs) (y:ys) = case (compare x y) of
           LT -> x : union  xs  (y:ys)
           EQ -> x : union  xs     ys
           GT -> y : union (x:xs)  ys
union  xs     []    = xs
union  []     ys    = ys
-- End of borrowing

smallPrimes  = primesW
               -- [2,3,5,7,11]
startFactors = zip smallPrimes (repeat 0)
maxFact = 13^2-1
-- For numbers 1 <= n < 13^2
factorise :: Integer -> Factors
factorise n | n < 1      = error "factorise: only positive integers allowed"
--            | n > maxFact= error ("factorise: only numbers <= "++ show maxFact ++ " allowed")
            | otherwise  = reverse (goFactors startFactors n [])

goFactors []         1 fs = fs
goFactors []         n fs = (n,1):fs  -- n is prime
goFactors ((p,m):ps) n fs = if mod == 0 then goFactors ((p,m+1):ps) n' fs
                          else goFactors ps n $
                               if m == 0 then fs else ((p,m):fs)
  where (n', mod) = divMod n p

primeFactors :: Factors -> [Integer]
primeFactors = map fst . filter ((0/=).snd)

groupsWithSamePrimeFactors =
  map (\ps@((l,_):_) -> (l, map snd ps)) $
  groupBy ((==) `on` fst) $
  sort $
  map (\n -> (primeFactors (factorise n), n)) [1..100]

haveSamePrimeFactors :: Integer -> Integer -> Bool
haveSamePrimeFactors = (==) `on` (primeFactors . factorise)

{-
[([],[1]),
([2],     [2,4,8,16,32,64]),
([2,3],   [6,12,18,24,36,48,54,72,96]),
([2,3,5], [30,60,90]),
([2,3,7], [42,84]),
([2,5],   [10,20,40,50,80,100]),
([2,7],   [14,28,56,98]),
([2,11],  [22,44,88]),
([2,13],  [26,52]),
([2,17],  [34,68]),
([2,19],  [38,76]),
([2,23],  [46,92]),
([3],     [3,9,27,81]),
([3,5],   [15,45,75]),
([3,7],   [21,63]),
([3,11],  [33,99]),
([5],     [5,25]),
([7],     [7,49]),

([11],    [11]),
([13],    [13]),
([17],    [17]),
([19],    [19]),
([2,29],  [58]),
([2,3,11],[66]),
([2,3,13],[78]),
([2,31],  [62]),
([2,37],  [74]),
([2,41],  [82]),
([2,43],  [86]),
([2,47],  [94]),
([2,5,7], [70]),
([23],    [23]),
([29],    [29]),
([3,13],  [39]),
([3,17],  [51]),
([3,19],  [57]),
([3,23],  [69]),
([3,29],  [87]),
([3,31],  [93]),
([31],    [31]),
([37],    [37]),
([41],    [41]),
([43],    [43]),
([47],    [47]),
([5,11],  [55]),
([5,13],  [65]),
([5,17],  [85]),
([5,19],  [95]),
([5,7],   [35]),
([53],    [53]),
([59],    [59]),
([61],    [61]),
([67],    [67]),
([7,11],  [77]),
([7,13],  [91]),
([71],    [71]),
([73],    [73]),
([79],    [79]),
([83],    [83]),
([89],    [89]),
([97],    [97])]
-}

eqPT []      []      = True
eqPT (i:is)  []      = False
eqPT []      (j:js)  = False
eqPT (i:is)  (j:js)  =  (i==j && eqPT is js)
                     || (primeFactors ifs == primeFactors jfs
                         && eqF (ifs, is) (jfs, js))
  where ifs = factorise i
        jfs = factorise j

-- Now we have two towers starting with two numbers with the same
-- prime factors but different multiplicities. These are equal only if
-- the prime factors match up exactly. Example:
--   eqF ([(2,1),(3,2)], is) ([(2,2),(3,1)], js)
-- iff
--   (eval is == 2*eval js) && (2*eval is == eval js)

eqF :: (Factors, PowerTower) -> (Factors, PowerTower) -> Bool
eqF (ifs, is) (jfs, js) = all (\(fi, fj) -> eqPert (fi, is) (fj, js)) fs
  where fs = zip (map snd ifs) (map snd jfs)
             -- keep only the multiplicities (assuming the primes are the same)

-- Thus we need a helper function to check equality perturbed by some
-- (small) factors (fi and fj).
-- Specification:  (fi*eval is == fj*eval js)
eqPert :: (Integer, PowerTower) -> (Integer, PowerTower) -> Bool
eqPert (0,  is)   (fj, js)   = 0 == fj
eqPert (fi, is)   (0,  js)   = 0 == fi
eqPert (fi, [])   (fj, js)   = eqAssym (factorise fi -/- factorise fj) js
eqPert (fi, is)   (fj, [])   = eqAssym (factorise fj -/- factorise fi) is
eqPert (fi, i:is) (fj, j:js) = (primeFactors lhs == primeFactors rhs)
                            && eqFPert fifs (ifs,is) fjfs (jfs,js)
  where fifs = factorise fi
        fjfs = factorise fj
        ifs  = factorise i
        jfs  = factorise j
        lhs  = fifs -*- ifs
        rhs  = fjfs -*- jfs

-- eqPert is a generalisation of eqPT (which is obtained with fi==fj==1)

eqFPert :: Factors -> (Factors, PowerTower) -> Factors -> (Factors, PowerTower) -> Bool
eqFPert fifs (ifs, is) fjfs (jfs, js) =
    all (\((mfi, mi),(mfj,mj)) -> eqPert2 (mi, is) (mfj-mfi) (mj, js)) fs
  where
    fs = zipF4 fifs ifs fjfs jfs
    -- for each prime factor, check mfi + mi*I == mfj + mj*J
    --                              mi*I == (mfj-mfi) + mj*J
    --                       where I = eval is
    --                       where J = eval js

-- check that i*eval is == m + j*eval js
-- Note that eqPert2 is a generalisation of eqPert (which is the special case m = 0)
eqPert2 :: (Integer, PowerTower) -> Integer -> (Integer, PowerTower) -> Bool
eqPert2 (a,is) m (b,js) | length (simplify is) <= 1 && length (simplify js) <= 1
                        = a*eval is == m + b*eval js
eqPert2 (a,is) 0 (b,js) = eqPert (a, is) (b, js)
eqPert2 (0,is) m (b,js) = m < 0 && eqAssym (factorise (negate m) -/- factorise b) js
eqPert2 (a,is) m (0,js) = m > 0 && eqAssym (factorise m -/- factorise a) is
eqPert2 (a,is) m (b,js) | null (simplify is)
                        = (a - m) > 0 && eqAssym (factorise (a-m) -/- factorise b) js
eqPert2 (a,is) m (b,js) | null (simplify js)
                        = (m + b) > 0 && eqAssym (factorise (m+b) -/- factorise a) is
eqPert2 (a,is) m (b,js) | length sis <= 1 && length sjs > 1
                        = lhs > 0 && eqAssym (factorise lhs -/- factorise b) js
  where sis = simplify is
        x   = eval sis     -- a*x == m + b*eval js
        lhs = a*x-m  -- Note that factorise may be called with large factors in lhs
        sjs = simplify js  -- (a*x - m) == b*eval js
eqPert2 (a,is) m (b,js) = error ("eqPert2 " ++ show (a,is) ++ " " ++ show m ++ " " ++ show (b,js))

-- TODO: complete the last case
{-
λ> findProblemseqPert2
*** Failed! Exception: 'eqPert2 (1,[2]) 1 (1,[2,2,7])' (after 4 tests and 169 shrinks):
Positive {getPositive = 1}
1
Positive {getPositive = 1}
[2]
[2,2,7]

-}

findProblemseqPert2 =
  quickCheck (\(Positive a) m (Positive j) ->
              forAllShrink genPT shPT $ \is ->
                forAllShrink genPT shPT $ \js ->
                  eqPert2 (a,is) m (j,js) `seq` True)

findProblemseqPT =
  quickCheck (forAllShrink genPT shPT $ \is ->
                forAllShrink genPT shPT $ \js ->
                  eqPT is js `seq` True)

-- check that prodexp xs == eval is
eqAssym :: Factors -> PowerTower -> Bool
eqAssym xs []      =  xs -=- []
eqAssym xs (i:is)  =  all nonNegMul xs
                   && (primeFactors xs == primeFactors ifs)
                   && all (\(mx, mi) -> eqAssym (factorise mx -/- factorise mi) is)
                          (zip (map snd xs) (map snd ifs))
  where ifs = factorise i

nonNegMul :: (Prime, Mult) -> Bool
nonNegMul (_p, m) = 0 <= m

zipF4 :: Factors -> Factors -> Factors -> Factors -> [((Mult,Mult),(Mult,Mult))]
zipF4 as bs cs ds = map (\p -> ((am p, bm p), (cm p, dm p))) combinedPrimes
  where combinedPrimes = merge (merge aps bps) (merge cps dps)
        [aps, bps, cps, dps] = map (map fst) [as, bs, cs, ds]
        [am,  bm,  cm,  dm ] = map (\xs-> \p-> maybe 0 id (lookup p xs)) [as, bs, cs, ds]

merge xs [] = xs
merge [] ys = ys
merge (x:xs) (y:ys) | x <= y     = x:merge xs (y:ys)
                    | otherwise  = y:merge (x:xs) ys

-- multiplication of Factors (is union of two bags of primes)
(-*-) :: Factors -> Factors -> Factors
(-*-) = unionWith (0/=) (+)
-- division is similar
(-/-) :: Factors -> Factors -> Factors
(-/-) = unionWith (0/=) (-)
(-^)  = power     -- Factor ^ Integer
(-=-) :: Factors -> Factors -> Bool
xs -=- ys = all ((0==).snd) (xs -/- ys)

unionWith :: Ord p => (m -> Bool) -> (m -> m -> m) ->
                      [(p, m)] -> [(p, m)] -> [(p, m)]
unionWith nonZ (+) = go
  where
    go [] fs = fs
    go fs [] = fs
    go ((p1,m1):fs1) ((p2,m2):fs2) = case compare p1 p2 of
      LT ->   (p1,m1   ) : go          fs1 ((p2,m2):fs2)
      EQ -> if nonZ (m1+m2) then
              (p1,m1+m2) : go          fs1          fs2
            else
                           go          fs1          fs2
      GT ->   (p2,   m2) : go ((p1,m1):fs1)         fs2


-- --------------
{-

The case is=[] is boring, is=i:is is more interesting

  fi*eval (i:is)
=
  prodexp (factors fi) * prodexp (factors (eval (i:is)))
=
  prodexp (factors fi -*- factors (eval (i:is)))
=
  prodexp (factors fi -*- (factors i -^ eval is))
-- similarly
  prodexp (factors fj -*- (factors j -^ eval js))
=
  fj*eval (j:js)

----

  fi*eval (i:is) == fj*eval (j:js)
=
  prodexp (factors fi -*- (factors i -^ eval is)) ==
  prodexp (factors fj -*- (factors j -^ eval js))
=
  factors fi -*- (factors i -^ eval is)  -=-
  factors fj -*- (factors j -^ eval js)
= let x = eval is; y = eval js
  factors fi -*- (factors i -^ x)  -=-
  factors fj -*- (factors j -^ y)
=
  all ((0==).snd) ((factors fi -*- (factors i -^ x)) -/-
                   (factors fj -*- (factors j -^ y))    )
-}




{-
  factors (eval is)
-- by case is

  factors (eval [])
==
  factors 1
==
  []

  factors (eval (i:is))
==
  factors (i^eval is)
==
  factors i -^ eval is

-}

-- ================================================================

{-
2016-01-06: Time to look for inequalities.

  eqPert2 (1,[2]) 1 (1,[12,93])
=
  1*eval [2] == 1 + 1*eval [12,93]
=
  2 == 1 + 12^93
=
  False

But not only are lhs/=rhs, they are not even close!

if we keep the invariant that the PowerTower sent to eval only
contains numbers > 1 then we can make some approximations very easily.

eval [] = 1 < 2 < 2^x <= i^x = eval (i:is)

-}

tetra :: (Eq a, Num a, Integral b) => b -> a -> b
tetra b 0 = 1
tetra b n = b^tetra b (n-1)

-- Don't try this on big PowerTowers!
prop_eval :: PowerTower -> Bool
prop_eval is = tetra 2 (length is) <= eval is

{-
Base case: is == []
  tetra 2 (length []) = tetra 2 0 = 1 = eval []
Step case:
  assume (tetra 2 (length is) <= eval is)

  tetra 2 (length (i:is))
=
  tetra 2 (1+length is)
=
  2^tetra 2 (length is)
<= -- induction hypothesis and monotonicity of (2^)
  2^eval is
<=
  i^eval is
=
  eval (i:is)
-}

{-
If we know the tower is non-empty we get a better result:

  eval (i:is) = i^eval is <= i^tetra 2 (length is)

----

Now we can get back to the check

  1*eval [2] == 1 + 1*eval [12,93]

the lhs is small, so evaluated completely to 2
the rhs >= 1 + 12^tetra 2 (length [93]) = 1 + 12^2 = 145

Thus lhs can't possible be equal to rhs.

This "assymetric" approximation will take care of many cases.

TODO: Step 1: implemented this for eqPert2
TODO: Step 2: generalise from equality to ordering

Step 1:
  first we need to be careful with evaluating tetra: it will also be too large for length is > 4
  For lhs < 65536 it is enough to cut the tetra evaluation at length around 4.

(We need to be careful not to approximate in "the wrong direction":
the rhs is made smaller and the lhs is made bigger.)

-}
