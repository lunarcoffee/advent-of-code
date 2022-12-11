import Control.Arrow
import Data.Char (isAlpha, isDigit)
import Data.List (foldl', transpose)
import Data.List.Split (chunksOf)

adjust :: (a -> a) -> Int -> [a] -> [a]
adjust f i = zipWith (\j -> if j == i then f else id) [0 ..]

runMoves :: (String -> String) -> ([String], [[Int]]) -> [String]
runMoves order = uncurry $ foldl' moveCrates
  where
    moveCrates state [n, src, dest] =
      adjust (crates ++) (dest - 1) $ adjust (drop n) (src - 1) state
      where
        crates = order $ take n $ state !! (src - 1)

main :: IO ()
main = do
  moves <- parse <$> getContents
  putStrLn $ map head $ runMoves reverse moves
  putStrLn $ map head $ runMoves id moves
  where
    parseState = map (filter isAlpha . (!! 1)) . chunksOf 4 . transpose
    parseMoves = map (map read . filter (all isDigit) . words) . tail
    parse = (parseState *** parseMoves) . span (/= "") . lines
