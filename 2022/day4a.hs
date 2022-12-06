import Data.List (isInfixOf, length)
import Data.List.Split (splitOn)

countSublists :: [[[Int]]] -> Int
countSublists = length . filter (\[a, b] -> isInfixOf a b || isInfixOf b a)

main :: IO ()
main = getContents >>= print . countSublists . parseRanges
  where
    parseRange = (\[x, y] -> [x .. y]) . map read . splitOn "-"
    parseRanges = map (map parseRange . splitOn ",") . lines
