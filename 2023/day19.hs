--- Day 19: Aplenty ---
--- compile with `ghc -O2 -package split day19.hs`

import Data.List.Split (splitOn, splitOneOf)
import Data.Either (lefts, rights, partitionEithers)
import qualified Data.Map as M

data Status = A | R deriving (Show, Eq)
data Rule = Rule Int Ordering Int Rule | Move String | End Status deriving (Show)
type Workflow = [Rule]
type WorkflowMap = M.Map String Workflow
type Rating = [Int]
type Ranges = [(Int, Int)]

evaluate :: String -> WorkflowMap -> Rating -> Status
evaluate label wfs rating = case evaluateRules rules rating of
  End status  -> status
  Move label' -> evaluate label' wfs rating
  where Just rules = M.lookup label wfs

evaluateRules :: [Rule] -> Rating -> Rule
evaluateRules (rule:rules) rating = case rule of
  Rule i comp n label -> evaluateRules next rating
    where next = if compare (rating!!i) n == comp then [label] else rules
  done -> done

accepted :: WorkflowMap -> [Rating] -> Int
accepted workflows = sum . map sum . filter ((==A) . evaluate "in" workflows)

findRanges :: WorkflowMap -> [(Ranges, String)] -> [Ranges]
findRanges wfs [] = []
findRanges wfs ((ranges, label):rest) = done ++ findRanges wfs (ranges' ++ rest)
  where (ranges', done) = partitionEithers $ processRule rules ranges
        Just rules = M.lookup label wfs

processRule :: [Rule] -> Ranges -> [Either (Ranges, String) Ranges]
processRule (rule:rules) ranges = case rule of 
  End A -> [Right ranges]
  End R -> []
  Move l -> [Left (ranges, l)]
  Rule i comp n r -> processRule (r:rules) (updateRanges left) ++
                     processRule rules (updateRanges right)
    where (left, right) = case comp of GT -> ((n+1, upper), (lower, n))
                                       LT -> ((lower, n-1), (n, upper))
          (lower, upper) = ranges !! i
          updateRanges r = take i ranges ++ [r] ++ drop (i+1) ranges

count = sum . map (product . map (\(i,j) -> j-i+1))
  
main :: IO ()
main = do
  contents <- readFile "input19.txt"
  let (workflows, ratings) = parse $ lines contents
  print $ accepted workflows ratings
  print $ count $ findRanges workflows [(take 4 $ repeat (1,4000), "in")]

parse :: [String] -> (WorkflowMap, [Rating])
parse ls = (M.fromList $ map parseWorkflow wf, map parseRating rt)
  where (wf:rt:_) = splitOn [""] ls

parseRating = map (read . drop 2) . splitOn "," . init . tail

parseWorkflow line = (name, map parseRule (splitOn "," rules))
  where (name:rules:_) = splitOneOf "{}" line

parseRule line
  | elem ':' line = Rule xmas compare (read num) labelOrStatus
  | line == "R" = End R
  | line == "A" = End A
  | otherwise   = Move line
  where ((var:comp:num):label:_) = (splitOn ":" line)
        xmas = case var of 'x' -> 0; 'm' -> 1; 'a' -> 2; 's' -> 3
        compare = case comp of '<' -> LT; '>' -> GT
        labelOrStatus = case label of "R" -> End R; "A" -> End A; l -> Move l

