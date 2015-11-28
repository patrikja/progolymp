module BFS where
import Data.Set hiding (map, filter)

data Sökproblem upos = Sökproblem {
  nästa   :: upos -> [upos],
  vinst   :: upos -> Bool
--  förlust :: upos -> Bool -- inbyggt i nästa
  }

bfs :: Ord a => Sökproblem a -> a -> [(Int, a)]
bfs sp = platta . numrera . bfs' sp empty

bfs' :: Ord a => Sökproblem a -> Set a -> a -> [[a]]
bfs' sp visited p | member p visited = []
                  | otherwise        = [p]:zipAppend (map (bfs' sp v') nexts)
  where nexts = nästa sp p
        v' = insert p visited

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

platta :: [(a, [b])] -> [(a, b)]
platta = concatMap (\(i,xs)-> map ((,)i) xs)
