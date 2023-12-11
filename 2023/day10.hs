--- Day 10: Pipe Maze ---

import Data.List
import Data.Maybe
import qualified Data.Map as M
import qualified Data.Set as S

type Pt = (Int, Int)
type Tile = Char
type PipeMaze = M.Map Pt Tile

neighbors :: PipeMaze -> Pt -> [Pt]
neighbors maze pos@(r,c) = 
  case M.lookup pos maze of
    Just 'S' -> filter (elem pos . neighbors maze) [(r-1,c),(r+1,c),(r,c-1),(r,c+1)]
    Just '|' -> inMaze [(r + 1, c), (r - 1, c)]
    Just '-' -> inMaze [(r, c - 1), (r, c + 1)]
    Just 'L' -> inMaze [(r - 1, c), (r, c + 1)]
    Just 'J' -> inMaze [(r - 1, c), (r, c - 1)]
    Just '7' -> inMaze [(r + 1, c), (r, c - 1)]
    Just 'F' -> inMaze [(r + 1, c), (r, c + 1)]
    _        -> []
  where inMaze = filter (flip M.member maze)

findLoop :: PipeMaze -> S.Set Pt
findLoop maze = search [start] S.empty
  where start = head $ M.keys $ M.filter ('S'==) maze
        search [] loop = loop
        search (pt:queue) loop = search (next++queue) (S.insert pt loop)
          where next = filter (flip S.notMember loop) $ neighbors maze pt

-- replace S with corresponding pipe and replace all non-loop tiles with '.'
replaceS :: S.Set Pt -> PipeMaze -> Pt -> Tile -> Tile
replaceS _ maze pos@(r,c) 'S'
  | path north && path south = '|'
  | path east  && path west = '-'
  | path north && path east = 'L'
  | path north && path west = 'J'
  | path south && path west = '7'
  | path south && path east = 'F'
  where (north, east, west, south) = ((r-1,c), (r,c+1), (r,c-1), (r+1,c))
        path = elem pos . neighbors maze
replaceS loop maze pos t
  | S.member pos loop = t
  | otherwise         = '.'

stretchHoriz :: PipeMaze -> PipeMaze
stretchHoriz maze = M.union stretched fillSpace
  where stretched = M.mapKeys (\(r,c) -> (r, c*2)) maze
        fillSpace = M.map newSym $ M.mapKeys (\(r,c) -> (r, c+1)) stretched
        newSym t
          | t == '-' || t == 'F' || t == 'L' = '-'
          | otherwise = ','

stretchVert :: PipeMaze -> PipeMaze
stretchVert maze = M.union stretched fillSpace
  where stretched = M.mapKeys (\(r,c) -> (r*2, c)) maze
        fillSpace = M.map newSym $ M.mapKeys (\(r,c) -> (r+1, c)) stretched
        newSym t
          | t == '|' || t == 'F' || t == '7' = '|'
          | otherwise = ','

flood :: PipeMaze -> Pt -> [Pt]
flood maze pos = search [pos] S.empty
  where search [] area = S.toList area
        search (pt@(r,c):stack) area
          | onEdge maxRow maxCol pt = [] 
          | otherwise = search (next++stack) (S.insert pt area)
          where next = filter (isDot . flip M.lookup maze) $
                       filter (flip S.notMember area) adj
                adj = [(r-1,c), (r+1,c), (r,c-1), (r,c+1)]
                isDot (Just tile) = tile == '.' || tile == ','
                isDot Nothing     = False
        maxRow = maximum $ map fst $ M.keys maze
        maxCol = maximum $ map snd $ M.keys maze

onEdge :: Int -> Int -> Pt -> Bool
onEdge rows cols (r,c) = r == 0 || c == 0 || r == rows-1 || c == cols-1

noEdge :: Int -> Int -> [Pt] -> Bool
noEdge rows cols = not . any (onEdge rows cols)

main :: IO ()
main = do
  contents <- readFile "input10.txt"
  let pipemaze = parse $ lines contents
  let mainLoop = findLoop pipemaze
  let fillmaze = (stretchHoriz . stretchVert) $ M.mapWithKey (replaceS mainLoop pipemaze) $ pipemaze
  print $ div (length mainLoop) 2
  let dots = M.keys $ M.filter ('.'==) fillmaze
  print $ length $ filter ((Just '.' ==) . flip M.lookup fillmaze) $
          fromJust $ find ([]/=) $ map (flood fillmaze) dots

parse :: [String] -> PipeMaze
parse = M.fromList . concat . zipWith addRow [0..] . map (zip [0..])
  where addRow row = map (\(col, tile) -> ((row, col), tile))
