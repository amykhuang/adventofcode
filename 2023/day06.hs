--- Day 6: Wait For It ---

waysToWinImpl :: Int -> (Int, Int) -> Int
waysToWinImpl holdFor (time, dist)
  | holdFor == time                   = 0
  | (time - holdFor) * holdFor > dist = 1 + recurse
  | otherwise                         = recurse
  where recurse = waysToWinImpl (holdFor + 1) (time, dist)

waysToWin :: (Int, Int) -> Int
waysToWin = waysToWinImpl 0

main :: IO ()
main = do
  contents <- readFile "input06.txt"
  let (time:dist:[]) = map (drop 1 . words) $ lines contents
  let races   = map (\(a, b) -> (read a, read b)) $ zip time dist
  let bigRace = (read $ concat time, read $ concat dist)
  print $ product $ map waysToWin races
  print $ waysToWin bigRace
