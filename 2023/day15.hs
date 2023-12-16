--- Day 15: Lens Library ---

import Data.Char (ord)
import Data.List (findIndex)
import Data.Map (Map, fromList, empty, assocs, adjust)
import Data.Text (pack, unpack, splitOn, strip)

hash :: String -> Int
hash = last . scanl (\x ch -> (mod (17 * (x + (ord ch))) 256)) 0

focusingPower :: Map Int [(String, Int)] -> Int
focusingPower = sum . map calc . assocs
  where calc (box, lens) = (box + 1) * (sum $ zipWith (*) [1..] $ map snd $ lens)

label :: [String] -> Int
label = label' (fromList $ zip [0..255] (repeat []))
  where label' m [] = focusingPower m
        label' m (s:ss) = label' (adjust fn (hash label) m) ss
          where fn | n == "-"   = remove label
                   | otherwise  = replace (label, read $ tail n)
                (label, n) = span (\c -> c /= '=' && c /= '-') s

replace :: (String, Int) -> [(String, Int)] -> [(String, Int)]
replace v@(label, _) ls = case findIndex ((label ==) . fst) ls of
  Just i  -> take i ls ++ [v] ++ drop (i + 1) ls
  Nothing -> ls ++ [v]

remove :: String -> [(String, Int)] -> [(String, Int)]
remove label = filter ((label /=) . fst)

main :: IO ()
main = do
  contents <- readFile "input15.txt"
  let steps = parse contents
  print $ sum $ map hash steps
  print $ label steps

parse :: String -> [String]
parse = map unpack . splitOn (pack ",") . strip . pack
