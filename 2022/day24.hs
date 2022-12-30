import Control.Lens
import Data.List (scanl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set

type Pos = (Int, Int)

adjFns :: [Pos -> Pos]
adjFns = (+~) <$> [_1, _2] <*> [-1, 1]

type Blizzards = Map Pos [Pos -> Pos]

minTripLength :: Blizzards -> (Int, Int) -> (Pos, Pos) -> (Int, Blizzards)
minTripLength v (mx, my) (from, to) = minTripLength' v (Set.singleton from) 0
  where
    minTripLength' v ps t
      | to `Set.member` ps = (t, v)
      | otherwise = minTripLength' v' ps' $ t + 1
      where
        ps' = Set.fromList $ filter isValid $ Set.toList ps >>= \p -> p : map (p &) adjFns
        isValid = (&&) <$> inRange <*> (`Map.notMember` v')
        v' = Map.fromListWith (++) $ Map.assocs v >>= uncurry (map . move)
    move p b = (bimap (`mod` mx) (`mod` my) $ b p, [b])
    inRange p@(x, y) = x >= 0 && y >= 0 && x < mx && y < my || p `elem` [from, to]

loopTrips :: (Blizzards, (Int, Int)) -> [Int]
loopTrips (v, bounds@(mx, my)) = loopMinLengths' (-1, 0) (mx, my - 1)
  where
    loopMinLengths' from to = map fst $ scanl' minTrip (0, v) $ cycle [(from, to), (to, from)]
    minTrip (_, v') = minTripLength v' bounds

main :: IO ()
main = do
  tripLengths <- loopTrips . parseBoth . map (init . tail) . init . tail . lines <$> getContents
  print $ tripLengths !! 1
  print $ sum $ take 4 tripLengths
  where
    parseBoth = (,) <$> Map.fromList . parseValley <*> parseBounds
    parseValley i@(r : _) =
      let parseCell c@(x, y) = (c,) . (: []) <$> lookup (i !! x !! y) (zip "^v<>" adjFns)
       in mapMaybe parseCell $ (,) <$> [0 .. length i - 1] <*> [0 .. length r - 1]
    parseBounds = (,) <$> length <*> length . head
