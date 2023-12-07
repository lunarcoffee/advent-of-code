import Data.Char (digitToInt, isDigit)
import Data.List (find, inits, isPrefixOf, isSuffixOf, tails)
import Data.Map qualified as M

parseLinePart1 :: String -> [Int]
parseLinePart1 = map digitToInt . filter isDigit

parseLinePart2 :: String -> [Int]
parseLinePart2 l = [firstDigit isPrefixOf $ tails l, firstDigit isSuffixOf $ reverse $ inits l]
  where
    firstDigit pred = (wordMap M.!) . head . concatMap (\s -> filter (`pred` s) strings)

    strings = words "one two three four five six seven eight nine 1 2 3 4 5 6 7 8 9"
    wordMap = M.fromList $ zip strings $ [1 .. 9] ++ [1 ..]

sumRowEnds :: [[Int]] -> Int
sumRowEnds = sum . map (\ds -> 10 * head ds + last ds)

main :: IO ()
main = do
  rows <- lines <$> getContents
  print $ sumRowEnds $ map parseLinePart1 rows
  print $ sumRowEnds $ map parseLinePart2 rows
