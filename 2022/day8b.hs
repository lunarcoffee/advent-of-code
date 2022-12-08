import Data.List (findIndex, inits, tails, transpose)

scenicScores :: [[Int]] -> [[Int]]
scenicScores = zipWith (zipWith (*)) <$> scoreLines <*> transpose . scoreLines . transpose
  where
    viewDistance (h : side) = maybe (length side) (+ 1) $ findIndex (>= h) side
    scoreSide sides = map viewDistance . sides
    scoreLines = map $ zipWith (*) <$> scoreSide (map reverse . tail . inits) <*> scoreSide tails

main :: IO ()
main = getContents >>= print . maximum . concat . scenicScores . parse
  where
    parse = map (map $ read . (: [])) . lines
