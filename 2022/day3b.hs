import Data.List (elemIndex, intersect)
import Data.List.Split (chunksOf)
import Data.Maybe (fromJust)

groupPriority :: [String] -> Int
groupPriority = (+ 1) . priority . findCommon
  where
    priority = fromJust . (`elemIndex` (['a' .. 'z'] ++ ['A' .. 'Z']))
    findCommon [a, b, c] = head $ intersect a $ intersect b c

main :: IO ()
main = getContents >>= print . sum . map groupPriority . chunksOf 3 . lines
