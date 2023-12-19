--- Day 17: Clumsy Crucible ---
-- compile with `ghc -O2 -package PSQueue day17.hs`

import Data.Array (Array, array, inRange, bounds, (!))
import qualified Data.PSQueue as PQ
import qualified Data.Set as S

type Pt = (Int, Int)
type City = Array Pt Int
type Node= (Int, Int, Int, Int)
type PQueue = PQ.PSQ Node Int

search :: Int -> Int -> City -> Int
search a b city = dijkstra S.empty (PQ.insert (1,1,3,1) 0 $ PQ.singleton (1,1,2,1) 0)
  where dijkstra visited queue
          | PQ.null queue = error "never reached the end"
          | (x, y) == (snd $ bounds city) && count >= a = distu
          | otherwise = dijkstra (S.insert u visited) (foldl update rest (getNext u))
          where Just ((u@(x,y,_,count) PQ.:-> distu), rest) = PQ.minView queue
                getNext = filter ((flip S.notMember) visited) . (neighbors a b city)
                update q v@(i,j,_,_) = case PQ.lookup v q of
                  Nothing -> if S.member v visited then q else q'
                  Just n  -> if n > distv then q' else q
                  where q' = PQ.insert v distv q
                        distv = distu + city ! (i, j)

-- directions: 1|up, 2|right, 3|down, 4|left
neighbors :: Int -> Int -> City -> Node -> [Node]
neighbors a b city (x, y, dir, count) = filter valid $ forward ++ turn
  where valid ((x,y,_,_)) = inRange (bounds city) (x,y)
        forward = if count < b then [move (x, y, dir, count+1)] else []
        turn = if count >= a then [move (x, y, (mod (dir-2) 4) + 1, 1),
                                   move (x, y, (mod dir 4) + 1, 1)] else []

move :: Node -> Node
move (x,y,dir,count) = case dir of
  1 -> (x-1, y, dir, count)
  2 -> (x, y+1, dir, count)
  3 -> (x+1, y, dir, count)
  4 -> (x, y-1, dir, count)

main :: IO ()
main = do
  contents <- readFile "input17.txt"
  let city = parse $ lines contents
  print $ search 0 3 city
  print $ search 4 10 city

parse :: [String] -> City
parse ls = array ((1,1),(ht,wd)) (zip pts $ concat $ map (map (read . (:[]))) ls)
  where pts = [(i, j) | i <- [1..ht], j <- [1..wd]]
        (ht, wd) = (length ls, length (head ls))
