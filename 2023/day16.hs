--- Day 16: The Floor Will Be Lava ---

import qualified Data.Map as M
import qualified Data.Set as S

type Pt = (Int, Int)
type Dir = (Int, Int)
type Beam = (Pt, Dir)
type Contraption = M.Map Pt Char
type Seen = S.Set Beam

continue :: (Dir -> Dir) -> Beam -> Beam
continue fn ((x, y), dir@(dx, dy)) = ((x + dx', y + dy'), dir')
  where dir'@(dx', dy') = fn dir

straight (dx, dy) = (dx, dy)
mirror1  (dx, dy) = (-1 * dy, -1 * dx)
mirror2  (dx, dy) = (dy, dx)

-- Follow a beam until it splits.
-- Return the two split beams, or [], with an updated seen set.
trace :: Contraption -> (Seen, Beam) -> (Seen, [Beam])
trace ct (seen, beam@(pos@(x,y),(dx, dy)))
  | M.notMember pos ct || S.member beam seen = (seen, [])
  | (tile == '.' || (tile == '-' && dx == 0) || (tile == '|' && dy == 0))
      = trace ct (seen', (continue straight beam))
  | tile == '/'  = trace ct (seen', (continue mirror1 beam))
  | tile == '\\' = trace ct (seen', (continue mirror2 beam))
  | tile == '-'  = (seen', [((x, y-1), (0,-1)), ((x, y+1), (0,1))])
  | tile == '|'  = (seen', [((x-1, y), (-1,0)), ((x+1,y), (1,0))])
  where Just tile = M.lookup pos ct
        seen' = S.insert beam seen

travel :: Contraption -> Beam -> Int
travel contrap beam = travel' (S.empty, [beam])
  where travel' (seen, []) = length $ S.map fst seen
        travel' (seen, beams) = travel' $ foldl update (seen, []) beams
        update (s, b) x = (s', b'++b) where (s', b') = trace contrap (s, x)

edgeBeams :: Contraption -> [Beam]
edgeBeams contrap = concat $ map (\i -> 
    [((0,i), (1,0)), ((l,i), (-1,0)), ((i,0), (0,1)), ((i,l), (0,-1))]
  ) [0..l]
  where l = (maximum $ map fst $ M.keys contrap) - 1

main :: IO ()
main = do
  contents <- readFile "input16.txt"
  let contraption = parse $ lines contents
  print $ travel contraption ((0,0), (0,1))
  print $ maximum $ map (travel contraption) $ edgeBeams contraption

parse :: [String] -> M.Map Pt Char
parse ls = M.fromList $ zip pts $ concat ls
  where pts = [ (i, j) | i <- [0..], j <- [0..length (head ls) - 1] ]
