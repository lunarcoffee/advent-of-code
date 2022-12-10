import Data.List (elemIndices, nub, scanl')

type Pos = (Int, Int)

runMove :: [Pos] -> Int -> [Pos]
runMove (h : knots) dir = scanl' follow (move h dir) knots
  where
    move (x, y) = (!!) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]
    follow (a, b) t@(x, y) =
      let (dx, dy) = (a - x, b - y)
       in if dx ^ 2 + dy ^ 2 <= 2 then t else (x + signum dx, y + signum dy)

countTailPositions :: Int -> [Int] -> Int
countTailPositions n = length . nub . map last . scanl' runMove (replicate n (0, 0))

main :: IO ()
main = do
  input <- parse <$> getContents
  print $ countTailPositions 2 input
  print $ countTailPositions 10 input
  where
    parseDir = head . flip elemIndices "LRUD" . head
    parse = concatMap (replicate <$> read . drop 2 <*> parseDir) . lines
