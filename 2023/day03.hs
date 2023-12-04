--- Day 3: Gear Ratios ---

import Data.List
import Data.Char
import qualified Data.Map as Map

data SymType = Digit | Sym | Dot deriving (Eq, Enum)

type Pt = (Int, Int)
type SymbolMap = Map.Map Pt Char
type GearList = [(Pt, [Int])]

getType :: Char -> SymType
getType c | isDigit c = Digit
          | c /= '.'  = Sym
          | otherwise = Dot

sameType :: (Pt, Char) -> (Pt, Char) -> Bool
sameType ((r1,_),c1) ((r2,_),c2) = getType c1 == getType c2 && r1 == r2

getAdjacent :: Pt -> [Pt]
getAdjacent (x,y) = [ (i,j) | i <- [x-1..x+1], j <- [y-1..y+1] ]

isAdjacent :: [Pt] -> Pt -> Bool
isAdjacent partPts symPt = any (== symPt) $ concat $ map getAdjacent partPts

nextToSym :: SymbolMap -> ([Pt], Int) -> Bool
nextToSym syms (pts, val) = any (flip Map.member syms) adjacentPts
  where adjacentPts = concat $ map getAdjacent pts

main :: IO ()
main = do
  contents <- readFile "input03.txt"
  let indexed = concat $ map toIndexed $ zip [0..] $ lines contents
  let symbolMap = Map.fromList $ filter ((== Sym) . getType . snd) indexed
  let numbers = map (\(pts, s) -> (pts, read s :: Int))
                $ map unzip
                $ filter (isDigit . snd . head)
                $ groupBy sameType indexed
  -- Part 1
  print $ sum $ map snd $ filter (nextToSym symbolMap) numbers
  -- Part 2
  let gearList = zip (Map.keys $ Map.filter (== '*') symbolMap) (repeat [])
  print $ sum $ map (foldl (*) 1) $ filter ((== 2) . length)
        $ map snd $ foldl updateGearMap gearList numbers
 
updateGearMap :: GearList -> ([Pt], Int) -> GearList
updateGearMap gears (parts, partVal) = 
  [if isAdjacent parts gear then (gear, partVal:adj) else old | old@(gear, adj) <- gears]

toIndexed :: (Int, String) -> [(Pt, Char)]
toIndexed (row, line) = map (\(col, c) -> ((row, col), c)) $ zip [0..] line


