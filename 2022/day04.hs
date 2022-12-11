import Data.List (foldl1', intersect, length)
import Data.List.Split (splitOn)

countContains :: [[[Int]]] -> Int
countContains = length . filter (elem =<< foldl1' intersect)

countOverlaps :: [[[Int]]] -> Int
countOverlaps = length . filter ((/= []) . foldl1' intersect)

main :: IO ()
main = do
  ranges <- parseRanges <$> getContents
  print $ countContains ranges
  print $ countOverlaps ranges
  where
    parseRange = (\[x, y] -> [x .. y]) . map read . splitOn "-"
    parseRanges = map (map parseRange . splitOn ",") . lines
