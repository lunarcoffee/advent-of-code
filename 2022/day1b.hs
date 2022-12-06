import Data.List (sortBy)
import Data.List.Split (splitOn)

sumTopInvs :: [[Int]] -> Int
sumTopInvs = sum . take 3 . sortBy (flip compare) . map sum

main :: IO ()
main = getContents >>= print . sumTopInvs . parseInvs
  where
    parseInvs = map (map read . lines) . splitOn "\n\n"
