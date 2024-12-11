--- Day 21: Step Counter ---

import Data.List (lookup)
import qualified Data.Set as S
import qualified Data.Map as M
import Data.Array (Array, array, assocs, indices, elems, inRange, bounds, (!), (//))
import Debug.Trace (trace)

type Pt = (Int, Int)
type Garden = Array Pt Char
type DPTable = [Array Pt (M.Map Pt Int)]

--reachedPlots :: Int -> Garden -> Pt -> Int
--reachedPlots steps garden start = length $ walk steps [start]
--  where walk 0 frontier = frontier
--        walk step frontier = walk (step - 1) frontier'
--            where frontier' = dedup $ concat $ map (neighbors garden) frontier
--        dedup = S.toList . S.fromList

neighbors :: Garden -> Pt -> [Pt]
neighbors garden pt@(x,y) = filter (\p -> garden ! p /= '#') $
                            filter (inRange (bounds garden)) $
                            [(x+1, y), (x-1,y), (x,y+1), (x,y-1)]

neighborsWrap :: Garden -> Pt -> [Pt]
neighborsWrap garden pt@(x,y) = filter (\p -> garden ! p /= '#') $
                                map (\(i,j) -> (wrap i, wrap j)) $
                                [(x+1, y), (x-1,y), (x,y+1), (x,y-1)]
  where (_, (l,_)) = bounds garden
        wrap x = (mod (x-1) l) + 1

--neighborsInf :: Garden -> (Pt, Int) -> [(Pt, Int)]
--neighborsInf garden (pt@(x,y), times) = 
--    filter (\(pt, _) -> garden ! pt /= '#') $
--    map wrap $ [(x+1, y), (x-1,y), (x,y+1), (x,y-1)]
--  where wrap (i,j) = -- trace ("   $" ++ show ((i,j), l))
--                     (((mod (i-1) l) + 1, (mod (j-1) l) + 1), if i > l || j > l then times + 1 else times)
--        (_, (l,_)) = bounds garden
--
--type GetNext = (Pt, Int) -> [(Pt, Int)]
--type Seen = M.Map Pt Int
--walk :: Int -> Int -> GetNext -> Seen -> Seen -> M.Map Pt Int -> M.Map Pt Int
--walk total steps getNext seenEven seenOdd frontier
--  | steps == total = M.unionWith (max) frontier seen
--  | otherwise = 
--      -- trace ("+" ++ show steps ++ ": " ++ show frontier')
--      walk total (steps + 1) getNext seenEven' seenOdd' frontier'
--  where seen = if isEven steps then seenEven else seenOdd
--        seenEven' = if isEven steps then M.unionWith (\a b -> max a b) frontier seenEven else seenEven
--        seenOdd' = if not (isEven steps) then M.unionWith (\a b -> max a b) frontier seenOdd else seenOdd
--        frontier' :: Seen = dedup $ filter (notSeen seen) $ concat $ map getNext (M.assocs frontier)
--        notSeen s (p, t) = M.notMember p s || (s M.! p) < t
--        dedup :: [(Pt, Int)] -> Seen
--        dedup = M.fromListWith (\a b -> max a b)
--
isEven n = mod n 2 == 0

neighborsInf :: Garden -> Pt -> [Pt]
neighborsInf garden pt@(x,y) =
    filter (\pt -> garden ! (wrap pt) /= '#') $
    [(x+1, y), (x-1,y), (x,y+1), (x,y-1)]
  where wrap (i,j) = ((mod (i-1) l) + 1, (mod (j-1) l) + 1)
        (_, (l,_)) = bounds garden


type GetNext = Pt -> [Pt]
walk :: Int -> Int -> GetNext -> S.Set Pt -> S.Set Pt -> [Pt] -> [[Pt]]
walk total steps getNext seenEven seenOdd frontier
  | steps == total = [frontier] -- dedup $ frontier ++ (S.toList seen) 
  | otherwise = 
      -- trace ("+" ++ show steps ++ ": " ++ show (dedup $ concat $ map getNext frontier)  ++ " " ++ show seen)
      frontier:(walk total (steps + 1) getNext seenEven' seenOdd' frontier')
  where seen = if isEven steps then seenOdd else seenEven
        seenEven' = if isEven steps then S.union (S.fromList frontier) seenEven else seenEven
        seenOdd' = if not (isEven steps) then S.union (S.fromList frontier) seenOdd else seenOdd
        frontier' = filter (flip S.notMember seen) $ dedup $ concat $ map getNext frontier
        dedup = S.toList . S.fromList

getResult :: Int -> [[Pt]] -> Int
getResult n pts
  | n >= length pts = error "list too short"
  | otherwise = if isEven n then rec (take (n+1) pts) else rec (tail (take (n+1) pts))
  where rec (a:b:rest) = length a + rec rest
        rec [a] = length a
        rec [] = 0

distToEach :: [[Pt]] -> M.Map Pt Int
distToEach pts = M.fromList $ concat $ map (\(ps, i) -> [(p,i) | p <- ps]) $ zip pts [0..]

reached :: Int -> Garden -> M.Map Pt Int -> Int
reached totalSteps garden dists = sum n
  where n = map (\d -> nrepeats totalSteps l d 1 0 (isEven d)) (M.elems dists)
        (_, (l,_)) = bounds garden

nrepeats :: Int -> Int -> Int -> Int -> Int -> Bool -> Int
nrepeats total gridSize numSteps count perimeterSize coparity
  | coparity == False = nrepeats total gridSize (numSteps+gridSize) count perimeterSize True
  | numSteps > total = trace ("done: " ++ show count) count
  | otherwise = trace (">" ++ show result) result
                where result = nrepeats total gridSize (numSteps + gridSize) (count + perimeterSize) (perimeterSize + 4) False

main :: IO ()
main = do
  contents <- readFile "input21.txt"
  let (start, garden) = parse $ lines contents
  let getNext = neighbors garden
  let x = 26501365
  let result = walk x 0 getNext (S.singleton start) S.empty [start]
  print $ getResult 64 result
  print $ getResult x result
  
  -- print $ filter ((==1) . snd) $ assocs $ walkInfinite garden (neighborsWrap garden) x (newTable start garden)
  -- print $ sum $ elems $ walk garden (neighborsWrap garden) x (newTable start garden)
  
parse :: [[Char]] -> (Pt, Garden)
parse ls = (start, array ((1,1), (l,l)) gardenList)
  where gardenList = zip pts (concat ls)
        Just start = lookup 'S' $ map (\(a,b) -> (b,a)) gardenList
        pts = [(i, j) | i <- [1..l], j <- [1..l]]
        l = length ls
