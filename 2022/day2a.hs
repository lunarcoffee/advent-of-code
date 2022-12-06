import Data.Char (ord)

roundScore :: (Int, Int) -> Int
roundScore = (+) <$> movePoints <*> resultPoints
  where
    resultPoints (a, b) = cycle [3, 6, 0] !! (b - a + 3)
    movePoints = (+ 1) . snd

main :: IO ()
main = getContents >>= print . sum . map roundScore . parseRounds
  where
    parseRounds = map (\[x, _, y] -> (ord x - ord 'A', ord y - ord 'X')) . lines
