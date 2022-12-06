import Data.List (intersect, length)
import Data.List.Split (splitOn)

main :: IO ()
main = getContents >>= print . countOverlaps . parseRanges
  where
    parseRange = (\[x, y] -> [x .. y] :: [Int]) . map read . splitOn "-"
    parseRanges = map (map parseRange . splitOn ",") . lines
    countOverlaps = length . filter ((/= []) . foldl1 intersect)
