import Data.List (elemIndices, foldl1', intersect)
import Data.List.Split (chunksOf)

bisect :: [a] -> [[a]]
bisect xs = map (($ xs) . ($ length xs `div` 2)) [take, drop]

groupPriority :: [String] -> Int
groupPriority = (+ 1) . priority . head . foldl1' intersect
  where
    priority = head . (`elemIndices` (['a' .. 'z'] ++ ['A' .. 'Z']))

main :: IO ()
main = do
  bags <- lines <$> getContents
  print $ sum $ map (groupPriority . bisect) bags
  print $ sum $ map groupPriority $ chunksOf 3 bags
