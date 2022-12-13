import Control.Lens
import Data.List (foldl', sort, stripPrefix)
import Data.Map qualified as Map

type Dir = Map.Map String Record

data Record = File Int | Dir Dir

dirSizes :: Record -> [Int]
dirSizes (File _) = []
dirSizes r@(Dir dir) = recordSize r : Map.foldl' (\sizes new -> sizes ++ dirSizes new) [] dir
  where
    recordSize (File size) = size
    recordSize (Dir dir) = sum $ Map.map recordSize dir

minAllowingUpdate :: [Int] -> Int
minAllowingUpdate dir = head . filter (>= head dir - 40_000_000) $ sort dir

main :: IO ()
main = do
  sizes <- dirSizes . parseFilesystem <$> getContents
  print $ sum $ filter (<= 100_000) sizes
  print $ minAllowingUpdate sizes
  where
    parseFilesystem = Dir . fst . foldl' parseLine (Map.empty, []) . lines
    parseLine (fs, cwd) line
      | Just dir <- stripPrefix "$ cd " line = case dir of
          "/" -> (fs, [])
          ".." -> (fs, init cwd)
          _ -> (makeDir dir, cwd ++ [dir])
      | [size, name] <- words line, size /= "$" = (makeFile name $ read size, cwd)
      | otherwise = (fs, cwd)
      where
        adjustPath = foldr (\d lens -> ix d %~ (Dir . lens . (\(Dir d) -> d)))
        makeDir dir = adjustPath (Map.insert dir $ Dir Map.empty) cwd fs
        makeFile name size = adjustPath (Map.insert name $ File size) cwd fs
