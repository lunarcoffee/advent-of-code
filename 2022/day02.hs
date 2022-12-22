import Data.Char (ord)

roundScoreMove :: (Int, Int) -> Int
roundScoreMove = (+) <$> (+ 1) . snd <*> resultPoints
  where
    resultPoints (a, b) = cycle [3, 6, 0] !! (b - a + 3)

roundScoreResult :: (Int, Int) -> Int
roundScoreResult = (+) <$> (* 3) . snd <*> movePoints
  where
    movePoints (a, b) = cycle [1 .. 3] !! (a + b + 2)

main :: IO ()
main = do
  rounds <- map parseRound . lines <$> getContents
  print $ sum $ map roundScoreMove rounds 
  print $ sum $ map roundScoreResult rounds 
  where
    parseRound [x, _, y] = (ord x - ord 'A', ord y - ord 'X')
