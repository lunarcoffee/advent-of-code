import Data.List (foldl1', intersect, length)
import Data.List.Split (splitOn)

countContains :: [[[Int]]] -> Int
countContains = length . filter (foldl1' intersect >>= elem)

countOverlaps :: [[[Int]]] -> Int
countOverlaps = length . filter ((/= []) . foldl1' intersect)

main :: IO ()
main = do
  input <- parseRanges <$> getContents
  print $ countContains input
  print $ countOverlaps input
  where
    parseRange = (\[x, y] -> [x .. y] :: [Int]) . map read . splitOn "-"
    parseRanges = map (map parseRange . splitOn ",") . lines
