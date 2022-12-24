import Data.Char (ord)
import Data.List (elemIndices)
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

type Pos = (Int, Int)

(@) :: [[a]] -> Pos -> a
hs @ (x, y) = hs !! x !! y

type Direction = (Pos -> Pos -> Bool) -> Pos -> Pos -> Bool

minPathLength :: [[Int]] -> [(Pos, Int)] -> Set Pos -> (Pos -> Bool) -> Direction -> Int
minPathLength hs ((src@(a, b), dist) : nexts) seen isDone dir
  | isDone src = dist
  | otherwise = minPathLength hs (nexts ++ adjs') seen' isDone dir
  where
    seen' = foldr Set.insert seen $ src : map fst adjs'
    adjs' = [(n, dist + 1) | n <- adjs, n `Set.notMember` seen, dir canWalk src n]
    canWalk src dest = hs @ dest - hs @ src <= 1
    adjs = filter inRange [(a - 1, b), (a + 1, b), (a, b - 1), (a, b + 1)]
    inRange (x, y) = and [x >= 0, y >= 0, x < length hs, y < length (head hs)]

distForwardBetween :: [[Int]] -> Pos -> Pos -> Int
distForwardBetween hs src dest = minPathLength hs [(src, 0)] Set.empty (== dest) id

distBackwardToMin :: [[Int]] -> Pos -> Int
distBackwardToMin hs dest = minPathLength hs [(dest, 0)] Set.empty ((== 0) . (hs @)) flip

main :: IO ()
main = do
  (heights, src, dest) <- parseMap . lines <$> getContents
  print $ distForwardBetween heights src dest
  print $ distBackwardToMin heights dest
  where
    parseMap = (,,) <$> map (map height) <*> coordsOf 'S' <*> coordsOf 'E'
    height c = fromMaybe (ord c - ord 'a') $ lookup c [('S', 0), ('E', 25)]
    coordsOf c = divMod <$> head . elemIndices c . concat <*> length . head
