--- Day 7: Camel Cards ---
import Data.List
import Control.Monad (replicateM)

handType :: String -> Int
handType hand
  | length a == 5               = 6  -- Five of a kind
  | length a == 4               = 5  -- Four of a kind
  | length a == 3 && secondPair = 4  -- Full house
  | length a == 3               = 3  -- Three of a kind
  | length a == 2 && secondPair = 2  -- Two pair
  | length a == 2               = 1  -- One pair
  | otherwise                   = 0  -- High card
  where (a:b) = (reverse . sortOn length . group . sort) hand
        secondPair = (length . head) b == 2

jokerHandType :: String -> Int
jokerHandType hand = handType $ last $ sortOn handType allPossibleHands
  where allPossibleHands = map (notJokers ++) $ replicateM n ('J':notJokers)
        notJokers = filter (/= 'J') hand
        n = length $ filter (== 'J') hand

compareSameType :: String -> String -> String -> Ordering
compareSameType _ "" "" = EQ
compareSameType cardOrder (a:handA) (b:handB)
  | indexA == indexB = compareSameType cardOrder handA handB
  | otherwise        = compare indexA indexB
  where Just indexA = elemIndex a cardOrder
        Just indexB = elemIndex b cardOrder

compareHand :: (String -> Int) -> String -> (String, Int) -> (String, Int) -> Ordering
compareHand handTypeFn cardOrder (a, _) (b, _)
  | typeA == typeB = compareSameType cardOrder a b
  | otherwise      = compare typeA typeB
  where (typeA, typeB) = (handTypeFn a, handTypeFn b)

totalWinnings :: [(String, Int)] -> Int
totalWinnings = sum . map (\(a, b) -> a * b) . zip [1..] . map snd

main :: IO ()
main = do
  contents <- readFile "input07.txt"
  let hands = map ((\(a:b:_) -> (a, read b)) . words) $ lines contents
  let sortedHands = sortBy (compareHand handType "123456789TJQKA") hands
  let jokerHands = sortBy (compareHand jokerHandType "J123456789TQKA") hands
  print $ totalWinnings sortedHands
  print $ totalWinnings jokerHands
