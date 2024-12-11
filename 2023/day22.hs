--- Day 22: Sand Slabs  ---
-- compile with `ghc -O2 -package split day22.hs`

import Data.List (minimumBy)
import Data.List.Split (splitOn)
import qualified Data.Map as M

type Pt = (Int, Int, Int)
type Brick = [Pt]
type Tetris = M.Map Int Brick

-- how to do
-- drop brick:
--   find the smallest z value for which no other bricks overlap.
--
-- option 1:
--   iterate through list of bricks; for each brick move it down as much as
--   possible until no more bricks move

-- option 2:
--   find the lowest brick, and drop it.
--     brick that contains the lowest z value? 
--   repeat. 

-- drop brick b as far a possible
dropBrick :: Int -> Tetris -> Tetris
dropBrick b t = t
  where brick = M.lookup b t

lowestBrick :: Tetris -> Int
lowestBrick = fst . minimumBy compareBrick . M.assocs

compareBrick :: (Int, Brick) -> (Int, Brick) -> Ordering
compareBrick (_, b1) (_, b2) = compare (lowestZ b1) (lowestZ b2)
  where lowestZ = minimum . map (\(_,_,z) -> z)

main :: IO ()
main = do
  contents <- readFile "example.txt"
  let tetris = M.fromList $ zip [0..] $ map parse $ lines contents
  print $ tetris
  print $ lowestBrick tetris

makeLine :: Pt -> Pt -> [Pt]
makeLine p1@(x1,y1,z1) p2@(x2,y2,z2)
  | p1 == p2 = [p1]
  | x1 /= x2 = [(x,y1,z1) | x <- [x1..x2]]
  | y1 /= y2 = [(x1,y,z1) | y <- [y1..y2]]
  | z1 /= z2 = [(x1,y1,z) | z <- [z1..z2]]

parse :: String -> Brick
parse s = makeLine (coord start) (coord end)
  where (start:end:[]) = splitOn "~" s
        coord t = let (a:b:c:[]) = map read (splitOn "," t) in (a,b,c)

