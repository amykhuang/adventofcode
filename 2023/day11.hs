--- Day 11: Cosmic Expansion ---

import Data.List

type Pt = (Int, Int)

expandRows :: [[Char]] -> [Int]
expandRows = map fst . filter (all ('.'==) . snd) . zip [0..]

galaxies :: [[Char]] -> [Pt]
galaxies image = map fst $ filter (('#'==) . snd) $ zip pts $ concat image
  where pts = [ (i, j) | i <- [0..], j <- [0..len]]
        len = length (head image) - 1

pairs :: [Pt] -> [(Pt, Pt)]
pairs pts = [(x, y) | (x:ys) <- tails pts, y <- ys]

distance :: Int -> [Int] -> [Int] -> (Pt, Pt) -> Int
distance n rows cols ((x1, y1), (x2, y2)) =
  yb - ya + xb - xa + (n-1) * (addSpace xs rows + addSpace ys cols)
  where addSpace (a,b) = length . filter (\x -> x < b && x > a)
        ys@(ya, yb) = (min y1 y2, max y1 y2)
        xs@(xa, xb) = (min x1 x2, max x1 x2)

main :: IO ()
main = do
  contents <- readFile "input11.txt"
  let space = lines contents
  let addRows = expandRows space
  let addCols = expandRows $ transpose space
  let solve n = sum . map (distance n addRows addCols) . pairs . galaxies
  print $ solve 2 space
  print $ solve 1000000 space
