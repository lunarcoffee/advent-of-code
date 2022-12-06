import Control.Arrow
import Data.Char (isAlpha, isNumber)
import Data.Foldable (toList)
import Data.List (foldl', transpose)
import Data.List.Split (chunksOf)
import qualified Data.Sequence as S

runMoves :: (S.Seq String, [[Int]]) -> S.Seq String
runMoves (state, moves) = foldl' moveCrates state moves
  where
    moveCrates state [n, src, dest] =
      S.adjust (crates ++) dest $ S.adjust (drop $ n + 1) src state
      where
        crates = reverse $ take (n + 1) $ S.index state src

main :: IO ()
main = getContents >>= putStrLn . toList . fmap head . runMoves . parse
  where
    parse = (S.fromList . parseState *** parseMoves . tail) . span (/= "") . lines
    parseState = map (filter isAlpha . (!! 1)) . chunksOf 4 . transpose
    parseMoves = map $ map (pred . read) . filter (all isNumber) . words
