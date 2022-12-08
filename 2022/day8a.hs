import Data.List (transpose)

visibleGrid :: [[Int]] -> [[Bool]]
visibleGrid = zipWith (zipWith (||)) <$> visibleLines <*> transpose . visibleLines . transpose
  where
    visibleSide scan post = zipWith (>) <*> post . scan max (-1)
    visibleLines = map $ zipWith (||) <$> visibleSide scanl id <*> visibleSide scanr tail

main :: IO ()
main = getContents >>= print . length . filter id . concat . visibleGrid . parse
  where
    parse = map (map $ read . (: [])) . lines
