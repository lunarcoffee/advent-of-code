import Control.Monad
import Data.List (elemIndices, nub, scanl')

type Pos = (Int, Int)

follow :: Pos -> Pos -> Pos
follow (a, b) t@(x, y) =
  let (dx, dy) = (a - x, b - y)
   in if dx ^ 2 + dy ^ 2 <= 2 then t else (x + signum dx, y + signum dy)

runMove :: [Pos] -> Int -> [Pos]
runMove ((hx, hy) : knots) dir =
  let newHead = [(hx - 1, hy), (hx + 1, hy), (hx, hy - 1), (hx, hy + 1)] !! dir
   in scanl' follow newHead knots

countTailPositions :: Int -> [Int] -> Int
countTailPositions n = length . nub . map last . scanl' runMove (replicate n (0, 0))

main :: IO ()
main = do
  moves <- parse <$> getContents
  print $ countTailPositions 2 moves
  print $ countTailPositions 10 moves
  where
    parseDir = head . flip elemIndices "LRUD" . head
    parse = replicate <$> read . drop 2 <*> parseDir <=< lines
