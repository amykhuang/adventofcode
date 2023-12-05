--- Day 5: If You Give A Seed A Fertilizer ---

import Data.List

mapTo :: [[Int]] -> Int -> Int
mapTo [] n = n
mapTo ((dest:src:range:[]):mappings) n
  | n >= src && n < src + range = n + dest - src
  | otherwise                   = mapTo mappings n

seedLocation :: [[[Int]]] -> Int -> Int
seedLocation [] n = n 
seedLocation (mapping:rest) n = seedLocation rest $ mapTo mapping n

edgePts :: [[[Int]]] -> [Int]
edgePts [] = []
edgePts (mapping:rest) = map (seedLocation rest) destPts ++ (edgePts rest)
                         where destPts = map (\l -> l!!1) mapping

isSeed :: [Int] -> Int -> Bool
isSeed [] _ = False
isSeed (start:range:seeds) pt
  | pt >= start && pt < start + range = True
  | otherwise = isSeed seeds pt

main :: IO ()
main = do
  contents <- readFile "input05.txt"
  let (seedStr:mapStr) = filter (/= [""]) $ groupBy lineBreak $ lines contents
  let seeds            = map read $ tail $ words $ head seedStr
  let almanac          = map (map (map read . words)) $ map (drop 1) mapStr

  -- (part 1) just follow each seed through the mappings
  print $ minimum $ map (seedLocation almanac) seeds

  -- (part 2) we only care about starting seeds that at some point map to a 
  -- edge point, since those mark the turning points (??) so find all
  -- edge points that started from a valid seed and only try those seeds
  let importantSeeds = filter (isSeed seeds) $ edgePts $ reverse almanac
  print $ minimum $ map (seedLocation almanac) $ importantSeeds

lineBreak a b = (a == "" && b == "") || (a /= "" && b /= "")
