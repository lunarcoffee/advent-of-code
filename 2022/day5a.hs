import Control.Arrow
import Data.Char (isAlpha, isDigit)
import Data.List (foldl', transpose)
import Data.List.Split (chunksOf)

adjust :: (a -> a) -> Int -> [a] -> [a]
adjust f i xs = take i xs ++ [f $ xs !! i] ++ drop (i + 1) xs

runMoves :: ([String], [[Int]]) -> [String]
runMoves = uncurry $ foldl' moveCrates
  where
    moveCrates state [n, src, dest] =
      adjust (crates ++) (dest - 1) $ adjust (drop n) (src - 1) state
      where
        crates = reverse $ take n $ state !! (src - 1)

main :: IO ()
main = getContents >>= putStrLn . map head . runMoves . parse
  where
    parseState = map (filter isAlpha . (!! 1)) . chunksOf 4 . transpose
    parseMoves = map (map read . filter (all isDigit) . words) . tail
    parse = (parseState *** parseMoves) . span (/= "") . lines
