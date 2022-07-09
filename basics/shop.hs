-- https://www.spoj.com/problems/SHOP/
module Main where

import qualified Data.Heap as H

main = interact $ unlines . solveList . breakList . words

type Position = (Int, Int)
type Graph = [String]
data Element = Element { val :: Int
                       , pos :: Position
                       }
  deriving (Ord, Eq, Show)

breakList :: [String] -> [Graph]
breakList ("0":"0":_) = []
breakList (x:y:xs) = [take (read y) xs] ++ breakList (drop (read y) xs)

solveList :: [Graph] -> Graph
solveList = map (show . solve)

solve :: Graph -> Int
solve g = compute [getSPos g] (H.fromList $ createElem 0 $ possiblePos' $ getSPos g) g
  where possiblePos' = possiblePos (length g, length (head g))

compute :: [Position] -> H.MinHeap Element -> Graph -> Int
compute vs heap g
          | elem p vs   = compute vs newHeap g
          | char == 'X' = compute (p:vs) newHeap g
          | char == 'D' = val e
          | otherwise   = compute (p:vs) (foldr H.insert newHeap (createElem (val e + read [char]) $ possiblePos' p)) g
  where Just e = H.viewHead heap
        p = pos e
        char = g !! (fst p) !! (snd p)
        newHeap = H.drop 1 heap
        possiblePos' = possiblePos (length (head g), length g)

createElem :: Int -> [Position] -> [Element]
createElem x = map (Element x)

possiblePos :: Position -> Position -> [Position]
possiblePos (m, n) (x, y) = filter (f) [(x - 1, y), (x, y + 1), (x + 1, y), (x, y - 1)]
  where f (a,b) = a >= 0 && b >= 0 && a < m && b < n

getSPos :: Graph -> Position
getSPos g = head $ filter ((/=(-1)).snd) 
              $ [(i, search 'S' (g !! i)) | i <- [0..]]

search :: (Eq a) => a -> [a] -> Int
search x xs = let index = length $ takeWhile (/=x) xs
              in if index == length xs then -1 else index

test = "0 7 4 23" == (unwords . solveList . breakList . words) testString
test1 = (unwords . solveList . breakList . words) testString

testString =
  " 2 2 \
  \ 1S \
  \ 2D \
  \ 5 4 \
  \ 1X2S1 \
  \ 21142 \
  \ 1X523 \
  \ D111X \
  \ 4 3 \
  \ X1S3 \
  \ 42X4 \
  \ X1D2 \
  \ 5 5 \
  \ S5213 \
  \ 2X2X5 \
  \ 51248 \
  \ 4X4X2 \
  \ 1445D \
  \ 0 0"
