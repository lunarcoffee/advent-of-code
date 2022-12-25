import Data.List (foldl1', intersect)
import Data.List.Split (splitOn)

isContaining, isOverlapping :: [[Int]] -> Bool
isContaining = elem =<< foldl1' intersect
isOverlapping = (/= []) . foldl1' intersect

main :: IO ()
main = do
  ranges <- map (map parseRange . splitOn ",") . lines <$> getContents
  print $ length $ filter isContaining ranges
  print $ length $ filter isOverlapping ranges
  where
    parseRange = (\[x, y] -> [x .. y]) . map read . splitOn "-"
