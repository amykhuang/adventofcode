--- Day 8: Haunted Wasteland ---

import qualified Data.Map as Map

type NodeMap = Map.Map String (String, String)

travel :: NodeMap -> String -> String -> Int
travel _ _ "ZZZ" = 0
travel nodeMap (dir:dirs) node = 1 + travel nodeMap dirs nextNode
  where nextNode = if dir == 'L' then left else right
        Just (left, right) = Map.lookup node nodeMap

ghostTravel :: NodeMap -> String -> String -> Int
ghostTravel _ _ (_:_:"Z") = 0
ghostTravel nodeMap (dir:dirs) node = 1 + ghostTravel nodeMap dirs nextNode
  where nextNode = if dir == 'L' then left else right
        Just (left, right) = Map.lookup node nodeMap

endsInA :: NodeMap -> [String]
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
parseNode s = (key, (left, init right))
  where (key, _:_:_:val)    = splitAt 3 s
        ('(':left, ',':_:right) = splitAt 4 val
