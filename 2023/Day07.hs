import Data.List (elemIndex, maximumBy, nub, sortOn)
import Data.Maybe (fromJust)
import Data.Ord (comparing)

type Cards = [Int]

type Hand = (Cards, Int)

parseInput :: String -> [String] -> [Hand]
parseInput cardOrder =
  let cardValue = fromJust . (`elemIndex` cardOrder)
   in map $ \l -> (map cardValue $ take 5 l, read $ drop 6 l)

count :: Eq a => [a] -> a -> Int
count xs x = length $ filter (== x) xs

handTypeScore :: Cards -> [Int]
handTypeScore cs = sortOn negate $ map (count cs) $ nub cs

handScore1, handScore2 :: Cards -> ([Int], Cards)
handScore1 cs = (handTypeScore cs, cs)
handScore2 cs =
  let noJokers = filter (/= 0) cs
      bestCard = maximumBy (comparing $ count noJokers) cs
   in (handTypeScore $ take 5 (noJokers ++ repeat bestCard), cs)

sumWinnings :: Ord a => (Cards -> a) -> [Hand] -> Int
sumWinnings key = sum . zipWith (*) [1 ..] . map snd . sortOn (key . fst)

main :: IO ()
main = do
  hands <- lines <$> getContents
  print $ sumWinnings handScore1 $ parseInput "23456789TJQKA" hands
  print $ sumWinnings handScore2 $ parseInput "J23456789TQKA" hands
