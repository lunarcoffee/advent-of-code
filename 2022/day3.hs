import Data.List (elemIndices, foldl1', intersect)
import Data.List.Split (chunksOf)

groupPriority :: [String] -> Int
groupPriority = (+ 1) . priority . head . foldl1' intersect
  where
    priorityMap = ['a' .. 'z'] ++ ['A' .. 'Z']
    priority = head . (`elemIndices` priorityMap)

main :: IO ()
main = do
  input <- lines <$> getContents
  print $ sum $ map (groupPriority . bisect) input
  print $ sum $ map groupPriority $ chunksOf 3 input
  where
    bisect xs = [take (mid xs) xs, drop (mid xs) xs]
    mid = (`div` 2) . length
