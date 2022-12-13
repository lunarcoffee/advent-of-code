import Data.List (elemIndices, foldl1', intersect)
import Data.List.Split (chunksOf)

groupPriority :: [String] -> Int
groupPriority = (+ 1) . priority . head . foldl1' intersect
  where
    priorityMap = ['a' .. 'z'] ++ ['A' .. 'Z']
    priority = head . (`elemIndices` priorityMap)

main :: IO ()
main = do
  bags <- lines <$> getContents
  print $ sum $ map (groupPriority . bisect) bags
  print $ sum $ map groupPriority $ chunksOf 3 bags
  where
    midIndex = (`div` 2) . length
    bisect xs = [take (midIndex xs) xs, drop (midIndex xs) xs]
