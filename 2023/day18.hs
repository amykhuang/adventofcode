--- Day 18: Lavaduct Lagoon ---

import Data.Map (empty, insertWith, assocs)
import Data.List (sort)
import Data.List.Split (chunksOf, divvy)
import Data.Maybe (mapMaybe)
import Data.Set (toList, fromList)

data Dir = R | D | L | U deriving (Show)
type DigPlan = [(Dir, Int)]
type Trench = [(Int, [Int])]

dig :: DigPlan -> Trench
dig plan = map (\(a,b) -> (a, sort b)) $ sort $ assocs $ trace (0,0) plan
  where trace _ [] = empty
        trace (x,y) ((dir,n):steps) = insertWith (++) x' [y'] (trace pt' steps)
          where pt'@(x', y') = case dir of R -> (x, y + n)
                                           D -> (x + n, y)
                                           L -> (x, y - n)
                                           U -> (x - n, y)

merge :: [(Int, Int)] -> [(Int, Int)]
merge ((i,j):(k,l):rest)
  | j == k    = merge ((i,l):rest)
  | otherwise = (i,j):(merge ((k,l):rest))
merge l = l

overlap :: [(Int, Int)] -> [Int] -> Bool -> (Int, Int)  -> Bool
overlap prevSegments segments isSegment (a, b)
  | isSegment     && isOverlap     = False
  | isSegment     && not isOverlap = True
  | not isSegment && isOverlap     = True
  | not isSegment && not isOverlap = False
  where isOverlap = any (\(i,j) -> a >= i && a <= j && b >= i && b <= j) prevSegments

disperse :: [Int] -> [(Int, Int)] -> [(Int, Int)]
disperse (pt:pts) ((a, b):segments)
  | pt > a && pt < b = [(a, pt), (pt, b)] ++ disperse pts segments
  | otherwise = (a, b):(disperse pts segments)
disperse _ segments = segments

fill :: (Int, [(Int, Int)]) -> Trench -> Int
fill _ [] = 1  -- add 1 to account for undercounting the segment on the first line
fill (row, ls) ((row', pts'):trench) =
  area + fill (row', merge $ sort $ (segments'++implicit')) trench
  where area = ((*(row'-row)) $ sum $ map (\(a,b) -> b-a+1) ls) +
               -- add extra area from the part of the row that protrudes
               (sum $ map (\(a, b) -> b - a) segments')
        -- real is all line segments between corner points, with previous points
        -- interspersed to break up the segments.
        -- The in/out status is swapped.
        segments' = filter noOverlap segments
        segments = disperse (fromTuples ls) $ toTuples $ chunksOf 2 $ pts'
        -- implicit is all other implicit segments that can be seen in this row.
        -- The in/out status stays the same.
        implicit' = filter hasOverlap implicit
        implicit = filter (flip notElem segments) $ toTuples $ divvy 2 1 $
                   deduplicate $ (pts' ++ fromTuples ls)
        hasOverlap = overlap ls pts' False
        noOverlap = overlap ls pts' True
        

toTuples = mapMaybe toTuple
  where toTuple (a:b:[]) = Just (a, b)
        toTuple _ = Nothing
fromTuples = concat . map (\(a, b) -> [a, b])
deduplicate = sort . toList . fromList

volume :: DigPlan -> Int
volume plan = fill start trench
  where trench = dig plan
        start = (fst $ head $ trench, [])

main :: IO ()
main = do
  contents <- readFile "input18.txt"
  let (plan, bigPlan) = unzip $ map parse $ lines contents
  print $ volume plan
  print $ volume bigPlan

parse :: String -> ((Dir, Int), (Dir, Int))
parse line = ((makeDir dir, read n), (makeDir dir', read ("0x"++n')))
  where (dir:n:color:[]) = words line
        (n', dir') = splitAt 5 (init $ tail $ tail $ color)
        makeDir c = case c of "R" -> R; "D" -> D; "L" -> L; "U" -> U
                              "0" -> R; "1" -> D; "2" -> L; "3" -> U
