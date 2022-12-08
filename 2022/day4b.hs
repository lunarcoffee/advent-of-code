import Data.List (intersect, length)
import Data.List.Split (splitOn)

countOverlaps :: [[[Int]]] -> Int
countOverlaps = length . filter ((/= []) . foldl1 intersect)

main :: IO ()
main = getContents >>= print . countOverlaps . parseRanges
  where
    parseRange = (\[x, y] -> [x .. y] :: [Int]) . map read . splitOn "-"
    parseRanges = map (map parseRange . splitOn ",") . lines
