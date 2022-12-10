import Data.Char (ord)

roundScoreMove :: (Int, Int) -> Int
roundScoreMove = (+) <$> movePoints <*> resultPoints
  where
    resultPoints (a, b) = cycle [3, 6, 0] !! (b - a + 3)
    movePoints = (+ 1) . snd

roundScoreResult :: (Int, Int) -> Int
roundScoreResult = (+) <$> resultPoints <*> movePoints
  where
    movePoints (a, b) = cycle [1 .. 3] !! (a + b + 2)
    resultPoints = (* 3) . snd

main :: IO ()
main = do
  rounds <- parseRounds <$> getContents
  print $ sum $ map roundScoreMove rounds
  print $ sum $ map roundScoreResult rounds
  where
    parseRounds = map (\[x, _, y] -> (ord x - ord 'A', ord y - ord 'X')) . lines
