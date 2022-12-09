import Data.List (foldl', sort, stripPrefix)
import qualified Data.Map as Map

type Dir = Map.Map String Record

data Record = File Int | Dir Dir

adjustAtPath :: (Dir -> Dir) -> [String] -> Dir -> Dir
adjustAtPath fn [] = fn
adjustAtPath fn (dir : path) = Map.adjust (Dir . adjustAtPath fn path . \(Dir d) -> d) dir

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
  input <- parseFilesystem <$> getContents
  print $ sum $ filter (<= 100_000) $ dirSizes input
  print $ minAllowingUpdate $ dirSizes input
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
        makeDir dir = adjustAtPath (Map.insert dir $ Dir Map.empty) cwd fs
        makeFile name size = adjustAtPath (Map.insert name $ File size) cwd fs
