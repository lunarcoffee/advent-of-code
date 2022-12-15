import Data.Function (on)
import Data.List (find)
import Data.List.Split (splitOn)
import Data.Set qualified as Set

type Pos = (Int, Int)

traceBlocked :: [[Pos]] -> (Set.Set Pos, Int)
traceBlocked = ((,) <*> maximum . Set.map snd) . foldr (Set.union . traceRock) Set.empty
  where
    traceRock = foldr (Set.union . traceLine) Set.empty . (zip <*> tail)
    traceLine ((a, b), (x, y)) = Set.fromList $ (,) <$> range a x <*> range b y
    range a b = [min a b .. max a b]

convergeBy :: Eq b => (a -> b) -> (a -> a) -> a -> a
convergeBy key = until =<< (((==) `on` key) =<<)

addSand :: Int -> Bool -> Set.Set Pos -> Set.Set Pos
addSand floor stopOnFloor bs
  | stopOnFloor && snd finalPos >= floor || Set.member (500, 0) bs = bs
  | otherwise = Set.insert finalPos bs
  where
    finalPos = convergeBy id step (500, 0)
    step p@(x, y) = maybe p step $ find isValid [(x, y + 1), (x - 1, y + 1), (x + 1, y + 1)]
    isValid p@(_, y) = Set.notMember p bs && y <= floor

sandToFill :: Int -> Bool -> Set.Set Pos -> Int
sandToFill floor stopOnFloor =
  let convergeBlocked = convergeBy Set.size $ addSand floor stopOnFloor
   in Set.size . (Set.difference =<< convergeBlocked)

main :: IO ()
main = do
  (blocked, floor) <- traceBlocked . parse <$> getContents
  print $ sandToFill floor True blocked
  print $ sandToFill (floor + 1) False blocked
  where
    parse = map (map (read . (++ ")") . ('(' :)) . splitOn " -> ") . lines
