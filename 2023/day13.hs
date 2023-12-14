--- Day 13: Point of Incidence ---

import Data.List

type Scape = [[Char]]

findMirror :: Int -> Scape -> Int
findMirror precision scape = case find (mirrored precision) lines of
    Just (a,b) -> length a
    Nothing    -> 0
  where lines = map (flip splitAt scape) [1..(length scape-1)]

mirrored :: Int -> (Scape, Scape) -> Bool
mirrored n (top, btm) = (==n) $ sum $ map countDiffs $ zip (reverse top) btm

countDiffs :: (String, String) -> Int
countDiffs (s1, s2) = sum $ map (\(a,b) -> if a == b then 0 else 1) $ zip s1 s2

summarize :: Int -> Scape -> Int
summarize n scape = 100 * (findMirror n scape) + findMirror n (transpose scape)

main :: IO ()
main = do
  contents <- readFile "input13.txt"
  let scapes = parse [] $ lines contents
  print $ sum $ map (summarize 0) scapes
  print $ sum $ map (summarize 1) scapes

parse :: Scape -> [String] -> [Scape]
parse currentScape [] = [currentScape]
parse currentScape (s:rest) = case s of
  "" -> currentScape:(parse [] rest)
  _  -> parse (currentScape ++ [s]) rest
