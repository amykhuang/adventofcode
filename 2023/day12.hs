--- Day 12: Hot Springs ---
-- compile with `ghc -O2 day12.hs -package MemoTrie`

import Data.List
import Data.MemoTrie
import qualified Data.Text as T

data Status = Valid | Some [Int] | Invalid deriving (Show)

update :: [[Char]] -> [Int] -> Bool -> Status
update [] arrs _ = Some arrs
update sprs [] _ = if elem '#' (concat sprs) then Invalid else Valid
update ['.':_] [arr] h = if h && arr /= 0 then Invalid else Some [arr]
update [s@('#':_)] [arr] _ = if arr >= length s then Some [arr-length s] else Invalid
update (spr@('.':_):springs) (arr:arrs) h
  | h && arr == 0 = update springs arrs False
  | h = Invalid
  | otherwise = update springs (arr:arrs) False 
update (spr@('#':_):springs) (arr:arrs) h
  | arr >= length spr = update springs ((arr - length spr):arrs) True
  | otherwise         = Invalid

type Memo f = f -> f
count :: Memo ((Bool, String, [Int]) -> Int)
count count (_, "", []) = 1
count count (_, "", [0]) = 1
count count (_, "", _) = 0
count count (_, sprs, []) = if elem '#' sprs then 0 else 1
count count (hash, springs, arrs) = case update (group chunk) arrs hash of
    Valid   -> 1
    Invalid -> 0
    Some a' -> if s' == "" then count (h', s', a')
               else count (h', '#':(tail s'), a') + count (h', '.':(tail s'), a')
  where (chunk, s') = span ('?' /=) springs
        h' = if chunk /= "" then last chunk == '#' else False
   
main :: IO ()
main = do
  contents <- readFile "input12.txt"
  let springs = map parse $ lines contents
  let solve = sum . map (memoFix count) . process
  print $ solve springs
  print $ solve $ map expand springs

process = zipWith (\a (b, c) -> (a, b, c)) (repeat False)

expand (springs, arrs) = (intercalate "?" $ take 5 $ repeat springs,
                          concat $ take 5 $ repeat arrs)

parse :: String -> ([Char], [Int])
parse s = (springs, arrangements rest)
  where arrangements = map (read . T.unpack) . T.splitOn (T.pack ",") . T.pack
        (springs:rest:[]) = words s
