import Data.List (findIndex, inits, tails, transpose)

zipTransposed :: (b -> b -> b) -> ([[a]] -> [[b]]) -> [[a]] -> [[b]]
zipTransposed zipFn mapFn = zipWith (zipWith zipFn) <$> mapFn <*> transpose . mapFn . transpose

visibleGrid :: [[Int]] -> [[Bool]]
visibleGrid = zipTransposed (||) visibleLines
  where
    visibleSide scan post = zipWith (>) <*> post . scan max (-1)
    visibleLines = map $ zipWith (||) <$> visibleSide scanl id <*> visibleSide scanr tail

scenicScores :: [[Int]] -> [[Int]]
scenicScores = zipTransposed (*) scoreLines
  where
    viewDistance (h : side) = maybe (length side) (+ 1) $ findIndex (>= h) side
    scoreSide sides = map viewDistance . sides
    scoreLines = map $ zipWith (*) <$> scoreSide (map reverse . tail . inits) <*> scoreSide tails

main :: IO ()
main = do
  input <- parse <$> getContents
  print $ length $ filter id $ concat $ visibleGrid input
  print $ maximum $ concat $ scenicScores input
  where
    parse = map (map $ read . (: [])) . lines
