import Data.Char (isDigit)
import Data.List (foldl', isPrefixOf, nub, sort, stripPrefix)

dirSizes :: [([String], Int)] -> [[String]] -> [Int]
dirSizes fs = map (\d -> sum [s | (path, s) <- fs, d `isPrefixOf` path]) . nub

minAllowingUpdate :: [Int] -> Int
minAllowingUpdate sizes = head . filter (>= maximum sizes - 40_000_000) $ sort sizes

main :: IO ()
main = do
  sizes <- uncurry dirSizes . parseFilesystem <$> getContents
  print $ sum $ filter (<= 100_000) sizes
  print $ minAllowingUpdate sizes
  where
    parseFilesystem = foldl' parseLine ([], [[]]) . lines
    parseLine s@(fs, cwds@(cwd : _)) line
      | Just dir <- stripPrefix "$ cd " line = case dir of
          "/" -> (fs, [] : cwds)
          ".." -> (fs, init cwd : cwds)
          _ -> let new = cwd ++ [dir] in (fs, new : cwds)
      | size : _ <- words line, all isDigit size = ((cwd, read size) : fs, cwds)
      | otherwise = s
