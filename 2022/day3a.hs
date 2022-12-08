import Data.List (elemIndices, intersect)

bagPriority :: (String, String) -> Int
bagPriority = (+ 1) . priority . head . uncurry intersect
  where
    priorityMap = ['a' .. 'z'] ++ ['A' .. 'Z']
    priority = head . (`elemIndices` priorityMap)

main :: IO ()
main = getContents >>= print . sum . map bagPriority . parseBags
  where
    bisect = (`div` 2) . length >>= splitAt
    parseBags = map bisect . lines
