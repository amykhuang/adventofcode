--- Day 3: Gear Ratios ---

import Data.List
import Data.Char
import qualified Data.Map as Map
import Data.Maybe

type Pt = (Int, Int)
data SymType = Digit | Sym | Dot deriving (Eq)

getType :: Char -> SymType
getType c | isDigit c = Digit
          | c == '.'  = Dot
          | otherwise = Sym

sameType :: (Pt, Char) -> (Pt, Char) -> Bool
sameType ((r1,_),c1) ((r2,_),c2) = getType c1 == getType c2 && r1 == r2

adjacentParts :: Map.Map Pt (Int, Int) -> Pt -> [Int]
adjacentParts partsMap (x, y) = map (snd . head)
   $ groupBy (\(id1,_) (id2,_) -> id1 == id2)
   $ mapMaybe (flip Map.lookup partsMap) [(i,j) | i<-[x-1..x+1], j<-[y-1..y+1]]

main :: IO ()
main = do
  contents <- readFile "input03.txt"
  let schematic = concat $ map parse $ zip [0..] $ lines contents
  let parts = Map.fromList
              $ concat
              $ map (\(id, (pts, n)) -> [(pt, (id, read n::Int)) | pt <- pts])
              $ zip [1, 2..]
              $ map unzip
              $ filter (isDigit . snd . head)
              $ groupBy sameType schematic 
  let symbols = map fst $ filter ((== Sym) . getType . snd) schematic
  let gears = map fst $ filter ((== '*') . snd) schematic
  print $ sum $ concat $ map (adjacentParts parts) symbols
  print $ sum $ map product $ filter ((== 2) . length) $ map (adjacentParts parts) gears
 
parse :: (Int, String) -> [(Pt, Char)]
parse (row, line) = map (\(col, c) -> ((row, col), c)) $ zip [0..] line
