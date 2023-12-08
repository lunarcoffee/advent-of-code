import Data.Char (digitToInt, isDigit)
import Data.List (inits, isPrefixOf, isSuffixOf, tails)
import Data.Map qualified as M

-- Represents one line of input, with the property that the head is the first number in the line
-- and the last element is the last number
type LineDigits = [Int]

-- `parseLine1` just returns all digits in the line
parseLine1, parseLine2 :: String -> LineDigits
parseLine1 = map digitToInt . filter isDigit
parseLine2 l = [firstDigit isPrefixOf $ tails l, firstDigit isSuffixOf $ reverse $ inits l]
  where
    -- Takes a `pred`icate (`isPrefixOf` or `isSuffixOf`) and a list of initial or final segments
    -- of the line (e.g. if `l` is "abc", ["abc", "bc", "c"] == `tails l` are its final segments
    -- and ["abc", "ab", "a"] == `reverse $ inits l` are its initial segments)
    --
    -- Intuitively, this is used to scan through the line starting from either end, returning the
    -- numerical value of the first digit (word or number) it finds. Scanning from both ends lets
    -- us avoid scanning the entire string, and works on inputs like "sevenine" where some
    -- one-sided approaches might fail
    firstDigit pred =
      -- `wordMap` is a map of words and numbers to their numeric value. Also, using `[1 ..]`
      -- (which is infinite) instead of `[1 .. 9]` works since Haskell is lazy, and because `zip`
      -- stops when either list ends
      let strings = words "one two three four five six seven eight nine 1 2 3 4 5 6 7 8 9"
          wordMap = M.fromList $ zip strings $ [1 .. 9] ++ [1 ..]
       in -- Let's say `pred` is `isPrefixOf` and we get the line's final segments. We go through
          -- each segment `s` in order, and for each one `filter` gives the list of digits that `s`
          -- starts with (really only at most one). Squash these together and the head is the first
          -- digit, which `wordMap` gives us the value for
          (wordMap M.!) . head . concatMap (\s -> filter (`pred` s) strings)

sumRowEnds :: [LineDigits] -> Int
sumRowEnds = sum . map (\ds -> 10 * head ds + last ds)

main :: IO ()
main = do
  -- `<$>` is an infix synonym of `fmap`. Since `getContents` returns an `IO String` and not a
  -- `String`, we can't just apply `lines` directly
  rows <- lines <$> getContents
  print $ sumRowEnds $ map parseLine1 rows
  print $ sumRowEnds $ map parseLine2 rows
