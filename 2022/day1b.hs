import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO ()
main = getContents >>= print . sumTopInvs . parseInvs
  where
    parseInvs = map (map read . lines) . splitOn "\n\n"
    sumTopInvs = sum . take 3 . reverse . sort . map sum
