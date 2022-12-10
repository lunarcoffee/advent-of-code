import Data.List (sort)
import Data.List.Split (splitOn)

main :: IO ()
main = do
  invSizes <- map sum . parseInvs <$> getContents
  print $ maximum invSizes
  print $ sum $ take 3 $ reverse $ sort invSizes
  where
    parseInvs = map (map read . lines) . splitOn "\n\n"
