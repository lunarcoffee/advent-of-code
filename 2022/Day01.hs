import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  invSizes <- map (sum . map read . lines) . splitOn "\n\n" <$> getContents
  print $ maximum invSizes
  print $ sum $ take 3 $ reverse $ sort invSizes
