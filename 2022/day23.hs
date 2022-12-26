import Data.Bifunctor (bimap)
import Data.List (find)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set

type Pos = (Int, Int)

dirs :: Pos -> [[Pos]]
dirs (x, y) = map ((`map` [-1 .. 1]) . (bimap (+ x) (+ y) .)) [(-1,), (1,), (,-1), (,1)]

runRound :: (Set Pos, Int) -> (Set Pos, Int)
runRound (es, dir) =
  let movingEs = mapMaybe withProposed $ filter (not . allOpen . concat . dirs) $ Set.toList es
      (new, old) = assocSets $ Map.filter (null . tail) $ Map.fromListWith (++) movingEs
   in (Set.union new $ Set.difference es old, (dir + 1) `mod` 4)
  where
    assocSets = (,) <$> Map.keysSet <*> Set.fromList . map head . Map.elems
    withProposed p = (,[p]) . (!! 1) <$> find allOpen ((drop <> take) dir $ dirs p)
    allOpen = all (`Set.notMember` es)

emptyTiles :: Set Pos -> Int
emptyTiles es = range fst * range snd - Set.size es
  where
    range = (+ 1) . ((-) <$> maximum <*> minimum) . (`Set.map` es)

main :: IO ()
main = do
  elves <- map fst . iterate runRound . (,0) . parseMap . lines <$> getContents
  print $ emptyTiles $ elves !! 10
  print $ length (takeWhile not $ zipWith (==) elves $ tail elves) + 1
  where
    parseMap ls = Set.fromList [(x, y) | (x, l) <- zip [0 ..] ls, (y, '#') <- zip [0 ..] l]
