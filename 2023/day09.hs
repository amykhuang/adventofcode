--- Day 9: Mirage Maintenance ---

parse :: String -> [[Int]]
parse = map (map read . words) . lines

diffs :: [Int] -> [Int]
diffs h = zipWith (-) (tail h) h

predict :: [Int] -> Int
predict ls
  | all (0 ==) ls = 0
  | otherwise = (last ls) + predict (diffs ls)

solve = sum . map predict

main :: IO ()
main = do
  contents <- readFile "input09.txt"
  let histories = parse contents
  print $ solve histories
  print $ solve $ map reverse histories
