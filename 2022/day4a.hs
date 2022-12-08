import Data.List (intersect, length)
import Data.List.Split (splitOn)

countContains :: [[[Int]]] -> Int
countContains = length . filter (foldl1 intersect >>= elem)

main :: IO ()
main = getContents >>= print . countContains . parseRanges
  where
    parseRange = (\[x, y] -> [x .. y] :: [Int]) . map read . splitOn "-"
    parseRanges = map (map parseRange . splitOn ",") . lines
