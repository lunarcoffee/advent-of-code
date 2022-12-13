import Data.Char (ord)
import Data.List (elemIndices)
import Data.Maybe (fromMaybe)

type Pos = (Int, Int)

type Direction = (Pos -> Pos -> Bool) -> Pos -> Pos -> Bool

(@) :: [[a]] -> Pos -> a
hs @ (x, y) = hs !! x !! y

pathLength :: [[Int]] -> [(Pos, Int)] -> [Pos] -> (Pos -> Bool) -> Direction -> Int
pathLength hs ((src@(a, b), dist) : dists) seen isDone dir
  | isDone src = dist
  | otherwise = pathLength hs (dists ++ newDists) (src : map fst newDists ++ seen) isDone dir
  where
    canWalk src dest = hs @ dest - hs @ src <= 1
    newDists = [(n, dist + 1) | n <- adjacent, n `notElem` seen, dir canWalk src n]
    inRange (x, y) = and [x >= 0, y >= 0, x < length hs, y < length (head hs)]
    adjacent = filter inRange [(a - 1, b), (a + 1, b), (a, b - 1), (a, b + 1)]

distForwardBetween :: [[Int]] -> Pos -> Pos -> Int
distForwardBetween hs src dest = pathLength hs [(src, 0)] [] (== dest) id

distBackwardToMin :: [[Int]] -> Pos -> Int
distBackwardToMin hs dest = pathLength hs [(dest, 0)] [] ((== 0) . (hs @)) flip

main :: IO ()
main = do
  (heights, src, dest) <- parseMap <$> getContents
  print $ distForwardBetween heights src dest
  print $ distBackwardToMin heights dest
  where
    height c = fromMaybe (ord c - ord 'a') $ lookup c [('S', 0), ('E', 25)]
    coordsOf c = divMod <$> head . elemIndices c . concat <*> length . head
    parseMap = ((,,) <$> map (map height) <*> coordsOf 'S' <*> coordsOf 'E') . lines
