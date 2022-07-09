-- https://www.spoj.com/problems/SHOP/
module Main where

-- import Data.PriorityQueue

main = interact $ unlines . computeList . breakList . words

data PQ = PQ { val :: Int
             , pos :: [(Int, Int)] }
  deriving (Ord, Eq, Show)

breakList :: [String] -> [[String]]
breakList ("0":"0":_) = []
breakList (x:y:xs) = [take (read y) xs] ++ breakList (drop (read y) xs)

computeList :: [[String]] -> [String]
computeList = map (\x -> show $ compute [addSToPQ x] x)

compute :: [PQ] -> [String] -> Int
compute ((PQ i (p:ps)):xs) ys = if currentElem == 'D' then i else 
                                  if currentElem == 'S' then compute (sort $ (map (PQ i) (nextPos (p:ps) ys)) ++ xs) ys else 
                                    compute (sort $ (map (PQ (i + read [currentElem])) (nextPos (p:ps) ys)) ++ xs) ys
  where currentElem = ys !! (snd p) !! (fst p)

nextPos :: [(Int, Int)] -> [String] -> [[(Int, Int)]]
nextPos p@((x, y):ps) ys = filter f [(x-1, y):p, (x, y-1):p, (x+1, y):p, (x, y+1):p]
  where f ((a, b):_) = a >= 0 && a < m && b >= 0 && b < n && (not $ elem (a,b) ps) && ys !! b !! a /= 'X'
        m = length $ head ys
        n = length ys

enqueue :: (Ord a) => a -> [a] -> [a]
enqueue x xs = sort (x:xs)

addSToPQ :: [String] -> PQ
addSToPQ xs = PQ 0 [(head $ filter (/=(-1)) y, length $ takeWhile (==(-1)) y)]
  where y = map (search 'S') xs



sort :: (Ord a) => [a] -> [a]
sort [] = []
sort (x:xs) = lessThanX ++ [x] ++ greaterThanX
  where lessThanX    = filter (<x) xs
        greaterThanX = filter (>x) xs

search :: (Eq a) => a -> [a] -> Int
search x xs = let index = length $ takeWhile (/=x) xs
              in if index == length xs then -1 else index


test = "0 7" == (unwords . computeList . breakList . words) testString

testString =
  " 2 2 \
  \ 1S \
  \ 2D \
  \ 5 4 \
  \ 1X2S1 \
  \ 21142 \
  \ 1X523 \
  \ D111X \
  \ 0 0"
