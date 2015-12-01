main = interact rawf
rawf::String->String
rawf s = unwords (rawg (words s))
rawg :: [String] -> [String]
rawg ws = map show (f (read (head ws)))
f :: Int -> [Int]
f n = [1..n]
