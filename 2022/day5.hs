import Control.Arrow
import Data.Char (isAlpha, isDigit)
import Data.List (foldl', transpose)
import Data.List.Split (chunksOf)

adjust :: (a -> a) -> Int -> [a] -> [a]
adjust f i xs = take i xs ++ [f $ xs !! i] ++ drop (i + 1) xs

runMoves :: (String -> String) -> ([String], [[Int]]) -> [String]
runMoves order = uncurry $ foldl' moveCrates
  where
    moveCrates state [n, src, dest] =
      adjust (crates ++) (dest - 1) $ adjust (drop n) (src - 1) state
      where
        crates = order $ take n $ state !! (src - 1)

main :: IO ()
main = do
  input <- parse <$> getContents
  putStrLn $ map head $ runMoves reverse input
  putStrLn $ map head $ runMoves id input
  where
    parseState = map (filter isAlpha . (!! 1)) . chunksOf 4 . transpose
    parseMoves = map (map read . filter (all isDigit) . words) . tail
    parse = (parseState *** parseMoves) . span (/= "") . lines
