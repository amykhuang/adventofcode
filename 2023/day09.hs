--- Day 9: Mirage Maintenance ---

parse :: (Monad m) => m String -> m [[Int]]
parse ms = ms >>= return . (map (map read . words) . lines)

diffs :: [Int] -> [Int]
diffs h = zipWith (-) (tail h) h

predict :: [Int] -> Int
predict ls
  | all (0 ==) ls = 0
  | otherwise = (last ls) + predict (diffs ls)

solve = sum . map predict

main :: IO ()
main = do
  histories <- parse $ readFile "input09.txt"
  print $ solve histories
  print $ solve $ map reverse histories
