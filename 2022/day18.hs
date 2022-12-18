import Control.Lens
import Data.Foldable (toList)
import Data.Ix (inRange)
import Data.Set qualified as Set

type Pos = (Int, Int, Int)

adjs :: Pos -> [Pos]
adjs p = map (p &) $ (%~) <$> [_1, _2, _3] <*> [pred, (+ 1)]

surfaceArea :: Set.Set Pos -> Int
surfaceArea ps = length $ filter (`Set.notMember` ps) $ adjs =<< toList ps

externalArea :: Set.Set Pos -> Int
externalArea ps = length $ filter (`Set.member` ps) $ adjs =<< toList (fill (x, y, z) Set.empty)
  where
    bounds@[(x, _), (y, _), (z, _)] = map range [_1, _2, _3]
    range get = ((,) <$> pred . minimum <*> (+ 1) . maximum) $ Set.map (^. get) ps
    fill p seen
      | isValid p = foldr fill (Set.insert p seen) $ adjs p
      | otherwise = seen
      where
        isValid p = inBounds p && p `Set.notMember` ps && p `Set.notMember` seen
        inBounds (x, y, z) = and $ zipWith inRange bounds [x, y, z]

main :: IO ()
main = do
  points <- parse <$> getContents
  print $ surfaceArea points
  print $ externalArea points
  where
    parse = Set.fromList . map (\c -> read $ '(' : c ++ ")") . lines
