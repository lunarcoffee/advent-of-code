import Data.List (elemIndex, intersect)
import Data.Maybe (fromJust)

bagPriority :: (String, String) -> Int
bagPriority = (+ 1) . priority . head . uncurry intersect
  where
    priority = fromJust . (`elemIndex` (['a' .. 'z'] ++ ['A' .. 'Z']))

main :: IO ()
main = getContents >>= print . sum . map bagPriority . parseBags
  where
    parseBags = map (flip splitAt <*> (`div` 2) . length) . lines
