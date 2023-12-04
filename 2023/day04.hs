--- Day 4: Scratchcards ---

import Data.List

matching :: ([Int], [Int]) -> Int
matching (wins, cards) = length $ intersect wins cards

score :: Int -> Int
score n = div (2 ^ n) 2

copyCards :: [Int] -> [Int] -> [Int]
copyCards [] _ = []
copyCards (m:matches) (c:count) = c:(copyCards matches (map (+c) l1 ++ l2))
  where (l1, l2) = splitAt m count

main :: IO ()
main = do
  contents <- readFile "input04.txt"
  let matches = map (matching . parse) $ lines contents
  print $ sum $ map score matches
  print $ sum $ copyCards matches (repeat 1)

parse :: String -> ([Int], [Int])
parse line = (toList winning, toList yours)
  where (winning, yours) = (splitOn '|' . snd . splitOn ':') line 
        toList = map read . split ' '

trim :: String -> String
trim = dropWhile (== ' ')

splitOn :: Char -> String -> (String, String)
splitOn delim ln = (trim s1, trim $ drop 1 s2) where (s1, s2) = span (/= delim) ln

split :: Char -> String -> [String]
split delim "" = []
split delim ln = [trim x] ++ split delim xs where (x, xs) = splitOn delim ln
