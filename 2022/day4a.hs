import Data.List (intersect, length)
import Data.List.Split (splitOn)

main :: IO ()
main = getContents >>= print . countContains . parseRanges
  where
    parseRange = (\[x, y] -> [x .. y] :: [Int]) . map read . splitOn "-"
    parseRanges = map (map parseRange . splitOn ",") . lines
    countContains = length . filter (flip elem <*> foldl1 intersect)
