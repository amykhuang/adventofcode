--- Day 9: Mirage Maintenance ---

import qualified Data.Text as T

diffs :: [Int] -> [Int]
diffs h = [ a - b | (a,b) <- zip (tail h) h ]

allDiffs :: [Int] -> [[Int]]
allDiffs = takeWhile (not . all (0==)) . iterate diffs

predict :: [Int] -> Int
predict = foldl (+) 0 . map last . reverse . allDiffs

predictBackwards :: [Int] -> Int
predictBackwards = foldl (flip (-)) 0 . map head . reverse . allDiffs

main :: IO ()
main = do
  contents <- readFile "input09.txt"
  let histories = map parse $ T.lines $ T.pack contents
  print $ sum $ map predict histories
  print $ sum $ map predictBackwards histories

parse :: T.Text -> [Int]
parse s = map (read . T.unpack) $ T.splitOn (T.pack " ") s
