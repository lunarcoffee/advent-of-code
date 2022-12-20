import Control.Lens
import Data.Bifunctor (first, second)
import Data.List (find, scanl')
import Data.Map qualified as Map
import Data.Maybe (fromJust, isJust)
import Data.Set qualified as Set

type Pos = (Int, Int)

type CaveState = (Set.Set Pos, [(Int, [Pos])], [(Int, Int)], Int, Int)

stepCave :: CaveState -> CaveState
stepCave (cave, (rIx, r) : rs, (jIx, j) : js, nr, h)
  | downR /= sideR = (cave, (rIx, downR) : rs, js, nr, h)
  | otherwise =
      let newTop = maximum $ h : map fst sideR
          spawnNext = rs & ix 0 . _2 %~ map (first (+ newTop))
       in (foldr Set.insert cave sideR, spawnNext, js, nr + 1, newTop)
  where
    [downR, sideR, _] = scanr ($) r [tryMove $ first pred, tryMove $ second (+ j)]
    tryMove dir r = let newR = map dir r in if any isInvalid newR then r else newR
    isInvalid p@(x, y) = p `Set.member` cave || y < 0 || y > 6

rocks :: [(Int, [Pos])]
rocks =
  let flat = map (4,) [2 .. 5]
      cross = [(4, 3), (5, 2), (5, 3), (5, 4), (6, 3)]
      corner = [(4, 2), (4, 3), (4, 4), (5, 4), (6, 4)]
      pole = map (,2) [4 .. 7]
      square = [(4, 2), (4, 3), (5, 2), (5, 3)]
   in cycle $ zip [0 ..] [flat, cross, corner, pole, square]

findCycle :: [(Int, Int)] -> Int -> (CaveState, Int, Int, Int, Int)
findCycle jets precision =
  let emptyState = (Set.fromList $ map (0,) [0 .. 6], rocks, jets, 0, 0)
      caches = scanl' cacheState (Map.empty, Nothing) $ iterate stepCave emptyState
   in fromJust $ snd $ fromJust $ find (isJust . snd) caches
  where
    cacheState s@(cache, cycleInfo) (cave, (rIx, r) : rs, jets@((jIx, _) : _), nr, h)
      | rIx /= jIx || isJust cycleInfo = s
      | otherwise = case Map.lookup key cache of
          Just (oldNr, oldH) ->
            let contState = (stackTop, (rIx, map normR r) : rs, jets, 0, 0)
             in (cache, Just (contState, oldNr, oldH, nr - oldNr, h - oldH))
          _ -> first (Map.insert key (nr, h)) s
      where
        key = (rIx, map normR r, stackTop)
        stackTop = Set.dropWhileAntitone ((< -precision) . fst) $ Set.map normR cave
        normR = _1 -~ h

stackHeight :: Int -> [(Int, Int)] -> Int -> Int
stackHeight n jets precision =
  let (contState, nr, h, dnr, dh) = findCycle jets precision
      cycleN = (n - nr) `div` dnr
      rRem = n - nr - cycleN * dnr
      topRem = fromJust (find ((>= rRem) . (^. _4)) $ iterate stepCave contState) ^. _5
   in h + cycleN * dh + topRem

main :: IO ()
main = do
  jets <- parse <$> getContents
  print $ stackHeight 2_022 jets 100
  print $ stackHeight 1_000_000_000_000 jets 100
  where
    parse = cycle . zip [0 ..] . map (\d -> if d == '<' then -1 else 1) . init
