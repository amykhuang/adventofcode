--- Day 2: Cube Conumdrum ---

import Data.List

data Game = Game Int [[(String, String)]] deriving (Show)

trim :: String -> String
trim = dropWhile (== ' ')

splitOn :: Char -> String -> (String, String)
splitOn delim ln = (s1, drop 1 s2) where (s1, s2) = span (/= delim) ln

split :: Char -> String -> [String]
split delim "" = [] 
split delim ln = [trim x] ++ split delim xs where (x, xs) = splitOn delim ln

getColor :: String -> [(String, String)] -> Int
getColor color ls = case find ((== color) . snd) ls of
  Just elem -> read (fst elem)
  Nothing   -> 0

countColors :: [[(String, String)]] -> (Int, Int, Int)
countColors rounds = (maximum $ map (getColor "red") rounds,
                      maximum $ map (getColor "blue") rounds,
                      maximum $ map (getColor "green") rounds)

validGame :: Game -> Int
validGame (Game id rounds)
  | red <= 12 && green <= 13 && blue <= 14 = id
  | otherwise = 0
  where (red, blue, green) = countColors rounds

minimumGame :: Game -> Int
minimumGame (Game _ rounds) = red * blue * green
  where (red, blue, green) = countColors rounds

parseGames :: String -> [[(String, String)]]
parseGames line = [[splitOn ' ' l | l <- split ',' ls] | ls <- split ';' line]
 
parse :: String -> Game
parse line = Game (read $ dropWhile (/=' ') id) (parseGames games)
             where (id, games) = splitOn ':' line

main :: IO ()
main = do
  contents <- readFile "input02.txt"
  let games = map parse $ lines contents
  print $ sum $ map validGame games
  print $ sum $ map minimumGame games
  

