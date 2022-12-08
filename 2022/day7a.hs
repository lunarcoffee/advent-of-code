import Data.List (stripPrefix)
import qualified Data.Map as Map

data Record = File Int | Dir Dir

type Dir = Map.Map String Record

adjustAtPath :: (Dir -> Dir) -> [String] -> Dir -> Dir
adjustAtPath fn [] = fn
adjustAtPath fn (dir : path) = Map.adjust (Dir . adjustAtPath fn path . \(Dir d) -> d) dir

allDirSizes :: Dir -> [Int]
allDirSizes = Map.foldl' collectSizes [0]
  where
    collectSizes (size : inners) (File new) = size + new : inners
    collectSizes (size : inners) (Dir new) =
      let newSizes = allDirSizes new
       in size + head newSizes : inners ++ newSizes

main :: IO ()
main = getContents >>= print . sum . filter (<= 100_000) . allDirSizes . parseFilesystem
  where
    parseFilesystem = fst . foldl parseLine (Map.empty, []) . lines
    parseLine (fs, cwd) line
      | Just dir <- stripPrefix "$ cd " line = case dir of
          ".." -> (fs, init cwd)
          "/" -> (fs, [])
          _ -> (makeDir dir, cwd ++ [dir])
      | Just dir <- stripPrefix "dir " line = (makeDir dir, cwd)
      | [size, name] <- words line, size /= "$" = (makeFile name $ read size, cwd)
      | otherwise = (fs, cwd)
      where
        makeDir dir = adjustAtPath (Map.insert dir $ Dir Map.empty) cwd fs
        makeFile name size = adjustAtPath (Map.insert name $ File size) cwd fs
