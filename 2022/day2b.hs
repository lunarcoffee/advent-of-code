import Data.Char (ord)

roundScore :: (Int, Int) -> Int
roundScore = (+) <$> resultPoints <*> movePoints
  where
    movePoints (a, b) = cycle [1 .. 3] !! (a + b + 2)
    resultPoints = (* 3) . snd

main :: IO ()
main = getContents >>= print . sum . map roundScore . parseRounds
  where
    parseRounds = map (\[x, _, y] -> (ord x - ord 'A', ord y - ord 'X')) . lines
