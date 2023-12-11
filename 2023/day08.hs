--- Day 8: Haunted Wasteland ---

import qualified Data.Map as Map

type Node = String
type Dir = Char
type NodeMap = Map.Map Node (Node, Node)

travel :: NodeMap -> String -> String -> Int
travel _ _ "ZZZ" = 0
travel nodeMap (dir:dirs) node = 1 + travel nodeMap dirs n
  where n = nextNode dir (Map.lookup node nodeMap)

ghostTravel :: NodeMap -> [Dir] -> Node -> Int
ghostTravel _ _ (_:_:"Z") = 0
ghostTravel nodeMap (dir:dirs) node = 1 + ghostTravel nodeMap dirs n
  where n = nextNode dir (Map.lookup node nodeMap)

nextNode :: Dir -> Maybe (Node, Node) -> Node
nextNode dir (Just (left, right)) = case dir of
  'L' -> left
  'R' -> right

endsInA :: NodeMap -> [Node]
endsInA = filter ((== 'A') . last) . Map.keys

lcms :: [Int] -> Int
lcms = foldl lcm 1

main :: IO ()
main = do
  contents <- readFile "input08.txt"
  let (dirs:_, _:nodesStr) = splitAt 1 $ lines contents
  let nodesMap = Map.fromList $ map parseNode nodesStr
  print $ travel nodesMap (cycle dirs) "AAA"
  print $ lcms $ map (ghostTravel nodesMap (cycle dirs)) $ endsInA nodesMap

parseNode :: String -> (String, (String, String))
parseNode s = (key, (tail $ init left, init right))
  where (key:_:left:right:_) = words s
