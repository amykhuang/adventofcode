--- Day 20: Pulse Propagation ---
--- compile with `ghc -O2 -package split day19.hs`

import Data.List.Split (splitOn)
import qualified Data.Map as M

type Module = String
data ModuleType = FlipFlop | Conjunction | Broadcast deriving (Show)
data Pulse = Low | High deriving (Show, Eq)
data Memory = On | Off | Mem (M.Map Module Pulse) deriving (Show, Eq)
type ModuleMap = M.Map Module (ModuleType, Memory)
type DestMap = M.Map Module [Module]

sendPulse ::  ModuleMap -> Module -> Pulse -> Module -> Maybe (ModuleMap, Pulse)
sendPulse mmap mod pulse sentFrom = case (M.lookup mod mmap, pulse) of
   (Nothing, _) -> Nothing
   (Just (Broadcast, _), _) -> Just (mmap, Low)
   (Just (FlipFlop, _), High) -> Nothing
   (Just (FlipFlop, On), _) -> Just (M.insert mod (FlipFlop, Off) mmap, Low)
   (Just (FlipFlop, Off), _) -> Just (M.insert mod (FlipFlop, On) mmap, High)
   (Just (Conjunction, Mem mem), _) ->
     Just (M.insert mod (Conjunction, Mem mem') mmap, p')
     where p' = if all (==High) (M.elems mem') then Low else High
           mem' = M.update (\_ -> Just pulse) sentFrom mem

pressButton :: DestMap -> ModuleMap -> ((Int, Int), ModuleMap)
pressButton dsts mmap = propagate [("broadcaster",Low,"")] ((1,0), mmap)
  where propagate [] x = x
        propagate ((m, p, from):frontier) ((ls, hs), mmap) =
          case sendPulse mmap m p from of
            Just (mmap',p') -> propagate (frontier ++ frontier') ((ls', hs'), mmap')
              where (ls', hs') = if p' == Low then (l + ls, hs) else (ls, l + hs)
                    l = length (dsts M.! m)
                    frontier' = [(d,p',m) | d <- dsts M.! m]
            Nothing -> propagate frontier ((ls, hs), mmap)

repeatPress :: DestMap -> Int -> ModuleMap -> Int
repeatPress dests n mmap = (sum (map fst result)) * (sum (map snd result))
  where result =  rep n mmap
        rep 0 m = []
        rep i m = (l',h'):(rep (i-1) m')
                  where ((l',h'), m') = pressButton dests m


-- part 2
-- it happens that each cycle starts from the beginning,
-- otherwise we would have to deal with offsets.
-- so just return the first time the condition happens
type Check = Module -> Pulse -> Bool

findCycle :: DestMap -> ModuleMap -> Module -> Int
findCycle dests mmap mod = rep 1 mmap
  where rep i m = case pressButtonUntil check dests m of
                    (True,  m') -> i
                    (False, m') -> rep (i + 1) m'
        check someMod sentPulse = someMod == mod && sentPulse == High

pressButtonUntil :: Check -> DestMap -> ModuleMap -> (Bool, ModuleMap)
pressButtonUntil check dsts mmap = propagate [("broadcaster",Low,"")] (False, mmap)
  where propagate [] x = x
        propagate ((m, p, from):frontier) (found, mmap) =
          case sendPulse mmap m p from of
            Just (mmap',p') -> propagate (frontier ++ frontier') (found || check m p', mmap')
              where frontier' = [(d,p',m) | d <- dsts M.! m]
            Nothing -> propagate frontier (found, mmap)


main :: IO ()
main = do
  contents <- readFile "input20.txt"
  let (dests, inputMods) = (\(a,b) -> (M.fromList a, M.fromList b)) $
                           unzip $ map parse $ lines contents
  let mods = populateConjunctionMem dests inputMods
  print $ repeatPress dests 1000 mods
  -- manually found conjuction modules that point to qb which points to rx
  print $ product $ map (findCycle dests mods) ["kv", "jg", "rz", "mr"]


populateConjunctionMem :: DestMap -> ModuleMap -> ModuleMap
populateConjunctionMem dsts mods = M.mapWithKey addInputs mods
  where addInputs mod (Conjunction, Mem mem) = 
          (Conjunction, Mem (foldl (\m x -> M.insert x Low m) mem $
                             M.keys $ M.filter (elem mod) dsts))
        addInputs _ x = x

parse :: String -> ((Module, [Module]), (Module, (ModuleType, Memory)))
parse line = ((name, destinations), (name, (ty, mem)))
  where ((t:s):dsts:[]) = splitOn " -> " line
        destinations = splitOn ", " dsts
        (ty, name, mem) = case t of '%' -> (FlipFlop, s, Off)
                                    '&' -> (Conjunction, s, Mem M.empty)
                                    _   -> (Broadcast, "broadcaster", On)
