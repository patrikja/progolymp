module BFS where
import Data.Set hiding (map, filter)

data Sökproblem upos = Sökproblem {
  nästa   :: upos -> [upos],
  vinst   :: upos -> Bool
  }

-- Bredden-först-sökning av ett sökproblem från en nod ger en lista
-- med noder parat med avståndet från startnoden.

bfs :: Ord a => Sökproblem a -> a -> [(Int, a)]
bfs sp = bfs' sp empty . sing

sing :: a -> [(Int, a)]
sing x = [(0,x)]

bfs' :: (Num i, Ord a) =>
  Sökproblem a -> Set a -> [(i, a)] -> [(i, a)]
bfs' sp visited [] = []
bfs' sp visited ((level, node):lns)
  | member node visited = bfs' sp visited lns
  | otherwise = (level, node) : bfs' sp v' (lns ++ nexts) -- TODO use efficient queues
      where nexts = map ((,) (level+1)) (nästa sp node)
            v' = insert node visited

{-
-- felaktig lösning som kan ge en massa dubletter
bfs :: Ord a => Sökproblem a -> a -> [(Int, a)]
bfs sp = platta . numrera . bfs' sp empty

bfs' :: Ord a => Sökproblem a -> Set a -> a -> [[a]]
bfs' sp visited p | member p visited = []
                  | otherwise        = [p]:zipAppend (map (bfs' sp v') nexts)
  where nexts = nästa sp p
        v' = insert p visited
-}
lösning :: Ord upos => Sökproblem upos -> upos -> Int
lösning sp = fst . head . filter (vinst sp . snd) . bfs sp

zipAppend :: [[[a]]] -> [[a]]
zipAppend [] = []
zipAppend (xs:xss) = combine xs (zipAppend xss)

combine :: [[a]] -> [[a]] -> [[a]]
combine [] ys = ys
combine xs [] = xs
combine (x:xs) (y:ys) = (x++y) : combine xs ys

numrera :: (Enum n, Num n) => [a] -> [(n,a)]
numrera = zip [0..]

-- Exempel: platta [(0,"ha"),(1,"du")] == [(0,'h'),(0,'a'),(1,'d'),(1,'u')]
platta :: [(a, [b])] -> [(a, b)]
platta = concatMap (\(i,xs)-> map ((,)i) xs)

----------------

-- Testa att söka "blint" åt alla håll i 2D efter diagonalen där rad == kolumn
testGraf :: Sökproblem (Int, Int)
testGraf = Sökproblem allaHåll samma
  where allaHåll (r,k) = [(r-1,k),(r+1,k),(r,k-1),(r,k+1)]
        samma (r,k) = r==k

test = print (lösning testGraf (20,0))
