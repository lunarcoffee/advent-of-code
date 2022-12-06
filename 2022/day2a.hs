import Data.List (elemIndex, isInfixOf)
import Data.Maybe (fromJust)

sumRounds :: [String] -> Int
sumRounds = sum . map ((+) <$> movePoints <*> winPoints)
  where
    movePoints = (+ 1) . fromJust . (`elemIndex` "XYZ") . last
    winPoints r
      | isInfixOf r "CXAYBZ" = 6
      | isInfixOf r "AXBYCZ" = 3
      | otherwise = 0

main :: IO ()
main = getContents >>= print . sumRounds . parseRounds
  where
    parseRounds = map (\[x, _, y] -> [x, y]) . lines
