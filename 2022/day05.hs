import Control.Lens
import Data.Char (isAlpha, isDigit)
import Data.List (foldl', transpose)
import Data.List.Split (chunksOf)

runMoves :: (String -> String) -> ([String], [[Int]]) -> [String]
runMoves order = uncurry $ foldl' moveCrates
  where
    moveCrates state [n, src, dest] =
      let crates = order $ take n $ state !! (src - 1)
       in state & ix (dest - 1) %~ (crates ++) & ix (src - 1) %~ drop n

main :: IO ()
main = do
  moves <- bimap parseState parseMoves . span (/= "") . lines <$> getContents
  putStrLn $ map head $ runMoves reverse moves
  putStrLn $ map head $ runMoves id moves
  where
    parseState = map (filter isAlpha . (!! 1)) . chunksOf 4 . transpose
    parseMoves = map (map read . filter (all isDigit) . words) . tail
