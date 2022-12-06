import Data.List (intersect, length)
import Data.List.Split (splitOn)

countOverlaps :: [[[Int]]] -> Int
countOverlaps = length . filter (\[a, b] -> intersect a b /= [])

main :: IO ()
main = getContents >>= print . countOverlaps . parseRanges
  where
    parseRange = (\[x, y] -> [x .. y]) . map read . splitOn "-"
    parseRanges = map (map parseRange . splitOn ",") . lines
