import Data.Char (digitToInt, isDigit)
import Data.List (inits, isPrefixOf, isSuffixOf, tails)
import Data.Map qualified as M

parseLine1 :: String -> [Int]
parseLine1 = map digitToInt . filter isDigit

parseLine2 :: String -> [Int]
parseLine2 l = [firstDigit isPrefixOf $ tails l, firstDigit isSuffixOf $ reverse $ inits l]
  where
    firstDigit pred =
      let strings = words "one two three four five six seven eight nine 1 2 3 4 5 6 7 8 9"
          wordMap = M.fromList $ zip strings $ [1 .. 9] ++ [1 ..]
       in (wordMap M.!) . head . concatMap (\s -> filter (`pred` s) strings)

sumRowEnds :: [[Int]] -> Int
sumRowEnds = sum . map (\ds -> 10 * head ds + last ds)

main :: IO ()
main = do
  rows <- lines <$> getContents
  print $ sumRowEnds $ map parseLine1 rows
  print $ sumRowEnds $ map parseLine2 rows
