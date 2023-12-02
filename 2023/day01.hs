--- Day 1: Trebuchet?! ---

import Data.Char
import Data.Maybe

readNumeric :: String -> Char
readNumeric "" = error "empty list"
readNumeric (x:xs) | isDigit x = x
                   | otherwise = readNumeric xs

firstLast :: (String -> Char) -> (String -> Char) -> String -> String
firstLast fn fnrev s = [fn s] ++ [fnrev $ reverse s]

readWord :: String -> Maybe Char
readWord "" = Nothing
readWord word@(x:xs)
  | word == "one"   = Just '1'
  | word == "two"   = Just '2'
  | word == "three" = Just '3'
  | word == "four"  = Just '4'
  | word == "five"  = Just '5'
  | word == "six"   = Just '6'
  | word == "seven" = Just '7'
  | word == "eight" = Just '8'
  | word == "nine"  = Just '9'
  | isDigit x       = Just x
  | otherwise = readWord xs

readWordB :: String -> Maybe Char
readWordB "" = Nothing
readWordB word@(x:xs)
  | word == "eno"   = Just '1'
  | word == "owt"   = Just '2'
  | word == "eerht" = Just '3'
  | word == "ruof"  = Just '4'
  | word == "evif"  = Just '5'
  | word == "xis"   = Just '6'
  | word == "neves" = Just '7'
  | word == "thgie" = Just '8'
  | word == "enin"  = Just '9'
  | isDigit x       = Just x
  | otherwise = readWordB xs

firstWord :: (String -> Maybe Char) -> String -> String -> Char
firstWord fn word "" = case fn word of
    Just d -> d
    _ -> error "no digit found" 
firstWord fn word (r:rs) = case fn word of
    Just d  -> d
    Nothing -> firstWord fn (word ++ [r]) rs

main :: IO ()
main = do
  contents <- readFile "input01.txt"
  let doc = lines contents
  print $ sum $ map (read . firstLast readNumeric readNumeric) doc
  print $ sum $ map (read . firstLast (firstWord readWord "" ) (firstWord readWordB "")) doc

