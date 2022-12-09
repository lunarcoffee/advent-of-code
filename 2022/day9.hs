import Data.List (elemIndices, nub, scanl')

type Pos = (Int, Int)

midpoint :: Pos -> Pos -> Pos
midpoint (x, y) (a, b) = (roundToward (i (a + x) / 2) $ i x, roundToward (i (b + y) / 2) $ i y)
  where
    roundToward x n = if x < n then ceiling x else floor x
    i = fromIntegral

runMove :: [Pos] -> Int -> [Pos]
runMove (h : knots) dir = scanl' follow (move h dir) knots
  where
    follow h@(a, b) t@(x, y) = if (x - a) ^ 2 + (y - b) ^ 2 <= 2 then t else midpoint h t
    move (x, y) = (!!) [(x - 1, y), (x + 1, y), (x, y - 1), (x, y + 1)]

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
