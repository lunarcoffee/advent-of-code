import Control.Monad
import Data.List (elemIndices, nub, scanl')

type Pos = (Int, Int)

follow :: Pos -> Pos -> Pos
follow (a, b) t@(x, y) =
  let (dx, dy) = (a - x, b - y)
   in if dx ^ 2 + dy ^ 2 <= 2 then t else (x + signum dx, y + signum dy)

runMove :: [Pos] -> Int -> [Pos]
runMove ((x, y) : knots) dir =
  let head' = [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)] !! dir
   in scanl' follow head' knots

countTailPositions :: Int -> [Int] -> Int
countTailPositions n = length . nub . map last . scanl' runMove (replicate n (0, 0))

main :: IO ()
main = do
  moves <- (parseMove <=< lines) <$> getContents
  print $ countTailPositions 2 moves
  print $ countTailPositions 10 moves
  where
    parseMove = replicate <$> read . drop 2 <*> parseDir
    parseDir = head . flip elemIndices "LRUD" . head
