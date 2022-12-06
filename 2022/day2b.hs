import Control.Applicative
import Data.Char (ord)
import Data.List (elemIndex)
import Data.Maybe (fromJust)

sumRounds :: [String] -> Int
sumRounds = sum . map ((+) <$> resultPoints <*> counterPoints)
  where
    moveIndex m = fromJust $ elemIndex m "XYZ" <|> elemIndex m "ABC"
    resultPoints = (* 3) . moveIndex . last
    counterPoints [a, b] = [3, 1, 2, 3, 1] !! newIndex
      where
        newIndex = moveIndex a + ord b - ord 'X'

main :: IO ()
main = getContents >>= print . sumRounds . parseRounds
  where
    parseRounds = map (\[x, _, y] -> [x, y]) . lines
