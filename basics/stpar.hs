-- https://www.spoj.com/problems/STPAR/
module Main where

main = interact $ unlines . map computeList . breakList . words

breakList :: [String] -> [[String]]
breakList ["0"] = []
breakList (x:xs) = [take (read x) xs] ++ breakList (drop (read x) xs)

computeList :: [String] -> String
computeList = compute [] 1 . map read

compute :: [Int] -> Int -> [Int] -> String
compute [] n [] = "yes"
compute [] n (y:ys) = if n == y then compute [] (n+1) ys else compute [y] n ys
compute (x:xs) n [] = if n == x then compute xs (n+1) [] else "no"
compute (x:xs) n (y:ys) 
  | x == n = compute xs (n+1) (y:ys)
  | y == n = compute (x:xs) (n+1) ys
  | x < y = "no"
  | otherwise = compute (y:x:xs) n ys

test = "yes no yes" == (unwords . map computeList . breakList . words) testString

testString = 
  " 5                    \
  \ 5 1 2 4 3            \
  \ 5                    \
  \ 4 1 5 3 2            \
  \ 10                   \
  \ 1 2 10 5 4 3 7 6 8 9 \
  \ 0                    "
