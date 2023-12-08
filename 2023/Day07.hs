import Data.List (elemIndex, maximumBy, nub, sortOn)
import Data.Maybe (fromJust)
import Data.Ord (comparing, Down (Down))

-- A list of five numbers representing a hand, like [1, 3, 3, 5, 13]. These numbers respect the
-- relative strength of each card
type Hand = [Int]

-- A player's hand along with their bid
type Player = (Hand, Int)

-- Parses the entire input given a string encoding the relative strengths of every card. The
-- numbers in a `Hand` are the indices of the corresponding symbol in `cardOrder`
parseInput :: String -> [String] -> [Player]
parseInput cardOrder =
  let cardValue = fromJust . (`elemIndex` cardOrder)
   in map $ \l -> (map cardValue $ take 5 l, read $ drop 6 l)

count :: Eq a => [a] -> a -> Int
count xs x = length $ filter (== x) xs

-- These three functions return a value representing the strength of a hand. This takes advantage
-- of the `Ord` instances for lists and tuples, which will compare two values by their first item,
-- moving onto the second item if the first are equal, and so on. When a list of hands is sorted on
-- these functions, they will be ordered by certain criteria

-- Returns a value that orders hands based solely on their type, disregarding card values
-- 
-- This creates a list of the frequencies of each card in the hand and sorts them in reverse. For
-- example, a full house corresponds to [3, 2] and two pairs correspond to [2, 2, 1]. Luckily, the
-- order of hand types by strength is exactly the order of these lists in Haskell
handTypeScore :: Hand -> [Int]
handTypeScore cs = sortOn Down $ map (count cs) $ nub cs

-- `handScore1` compares on type, then on the actual cards in the hand
handScore1, handScore2 :: Hand -> ([Int], Hand)
handScore1 cs = (handTypeScore cs, cs)
handScore2 cs =
  -- The optimal use for jokers is to treat them as the most common other card in the hand. Note
  -- that if there are multiple such cards, it doesn't matter which we choose, since jokers have
  -- their own value when comparing directly. Here, we first compare on the score of the
  -- theoretical best hand (created by replacing the jokers with the most common card), then on the
  -- actual hand (in which the jokers are treated as the weakest)
  let noJokers = filter (/= 0) cs
      bestCard = maximumBy (comparing $ count noJokers) cs
   in (handTypeScore $ take 5 (noJokers ++ repeat bestCard), cs)

sumWinnings :: Ord a => (Hand -> a) -> [Player] -> Int
sumWinnings key = sum . zipWith (*) [1 ..] . map snd . sortOn (key . fst)

main :: IO ()
main = do
  hands <- lines <$> getContents
  print $ sumWinnings handScore1 $ parseInput "23456789TJQKA" hands
  print $ sumWinnings handScore2 $ parseInput "J23456789TQKA" hands
