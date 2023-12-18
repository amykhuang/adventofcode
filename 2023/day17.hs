--- Day 17: Clumsy Crucible ---
-- compile with `ghc -O2 -package PSQueue day17.hs`

import qualified Data.Array as A
import qualified Data.PSQueue as PQ
import qualified Data.Set as S

type Pt = (Int, Int)
type City = A.Array Pt Int
type Vertex = (Int, Int, Int, Int)
type PQueue = PQ.PSQ Vertex Int

search :: (Vertex -> [(Vertex, Int)]) -> Pt -> Int
search next end = dijkstra S.empty (PQ.insert (1,1,3,1) 0 $ PQ.singleton (1,1,2,1) 0)
  where dijkstra visited queue
          | PQ.null queue = error "never reached the end"
          | (x, y) == end = distu
          | otherwise = dijkstra visited' (foldl update rest (getNext u))
          where Just ((u@(x,y,_,_) PQ.:-> distu), rest) = PQ.minView queue
                getNext = filter ((flip S.notMember) visited . fst) . next
                visited' = S.insert u visited 
                update q (v, cost) = case PQ.lookup v q of
                  Nothing -> if S.member v visited then q else q'
                  Just n -> if n > distu + cost then q' else q
                  where q' = PQ.insert v (distu + cost) q

-- directions: 1|up, 2|right, 3|down, 4|left
neighbors :: Int -> Int -> City -> Vertex -> [(Vertex, Int)]
neighbors a b city (x, y, dir, count) = map addCost $ filter valid $ forward ++ turn
  where valid ((x,y,_,_)) = A.inRange (A.bounds city) (x,y)
        addCost p@(x,y,_,_) = (p, city A.! (x,y))
        forward = if count < b then [move (x, y, dir, count+1)] else []
        turn = if count >= a then
          [move (x, y, (mod (dir-2) 4) + 1, 1),
           move (x, y, (mod dir 4) + 1, 1)] else []

move :: Vertex -> Vertex
move (x,y,dir,count) = case dir of
  1 -> (x-1, y, dir, count)
  2 -> (x, y+1, dir, count)
  3 -> (x+1, y, dir, count)
  4 -> (x, y-1, dir, count)

main :: IO ()
main = do
  contents <- readFile "input17.txt"
  let city = parse $ lines contents
  let (start, end) = A.bounds city
  print $ search (neighbors 0 3 city) end
  print $ search (neighbors 4 10 city) end

parse :: [String] -> City
parse ls = A.array ((1,1),(ht,wd)) (zip pts $ concat $ map (map (read . (:[]))) ls)
  where pts = [(i, j) | i <- [1..ht], j <- [1..wd]]
        (ht, wd) = (length ls, length (head ls))
