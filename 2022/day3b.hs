import Data.List (elemIndices, intersect)
import Data.List.Split (chunksOf)

groupPriority :: [String] -> Int
groupPriority = (+ 1) . priority . head . foldl1 intersect
  where
    priorityMap = ['a' .. 'z'] ++ ['A' .. 'Z']
    priority = head . (`elemIndices` priorityMap)

main :: IO ()
main = getContents >>= print . sum . map groupPriority . chunksOf 3 . lines
