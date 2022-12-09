import Data.List (sort)
import Data.List.Split (splitOn)

sumTopInvs :: [[Int]] -> Int
sumTopInvs = sum . take 3 . reverse . sort . map sum

main :: IO ()
main = do
  input <- parseInvs <$> getContents
  print $ maximum $ map sum input
  print $ sumTopInvs input
  where
    parseInvs = map (map read . lines) . splitOn "\n\n"
