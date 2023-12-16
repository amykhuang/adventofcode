--- Day 14: Parabolic Reflector Dish ---

import Data.List

type Pt = (Int, Int)
type Rocks = ([[Int]], [[Int]])

tilt :: Int -> Rocks -> Rocks
tilt _ ([], []) = ([], [])
tilt l (x:xs, o:os) = (x:xs, (roll o):os')
  where (_, os') = tilt l (xs, os)
        roll = concat . map stack . group .
               map (\o -> case find (>o) (reverse x) of Just i -> i; _ -> l+1)
        stack ls@(l:_) = take (length ls) [l - 1, l - 2..]

rotate :: Int -> Rocks -> Rocks
rotate l (xs, os) = ((convert . rotate') xs, (convert . rotate') os)
  where convert ls = map (\i -> map fst (filter ((==i) . snd) ls)) [1..l]
        rotate' = concat . zipWith (\r -> map (\c -> (r,c))) [l,l-1..1]

load :: Rocks -> Int
load (_, os) = sum $ concat os

doCycle :: Int -> Rocks -> Rocks
doCycle l = last . take 5 . iterate (rotate l . tilt l)

totalLoad :: Int -> Int -> [Rocks] -> Rocks -> Int
totalLoad l i seen state = case elemIndex state seen of
  Just start -> load (seen !! index)
    where index = start + (mod (1000000000 - start) (i - start))
  Nothing    -> totalLoad l (i+1) (seen ++ [state]) (doCycle l state)

main :: IO ()
main = do
  contents <- readFile "input14.txt"
  let input = map parse $ transpose $ lines contents
  let rocks = (map fst input, map snd input)
  let l = length input
  print $ load $ tilt l rocks
  print $ totalLoad l 0 [] rocks

parse :: String -> ([Int], [Int])
parse s = (map fst xs, map fst os)
  where (xs, os) = partition (('#'==) . snd) $ filter (('.'/=) . snd) $
                   zip [length s, length s - 1..] s

display :: Int -> Rocks -> String
display l (xs, os) = intercalate "\n" lines ++ "\n\n"
  where lines = transpose $ map showLine (zip xs os)
        showLine (xs, os) = [ if elem i xs then '#' else if elem i os then 'O' else '.' | i <- [l,l-1..1]]
