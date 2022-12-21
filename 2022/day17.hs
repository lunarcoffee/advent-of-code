import Control.Lens
import Control.Monad (guard)
import Data.List (find)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe)
import Data.Set qualified as Set

type Pos = (Int, Int)

rocks :: [(Int, [Pos])]
rocks =
  let flat = map (4,) [2 .. 5]
      cross = [(4, 3), (5, 2), (5, 3), (5, 4), (6, 3)]
      corner = [(4, 2), (4, 3), (4, 4), (5, 4), (6, 4)]
      pole = map (,2) [4 .. 7]
      square = [(4, 2), (4, 3), (5, 2), (5, 3)]
   in cycle $ zip [0 ..] [flat, cross, corner, pole, square]

type CaveState = (Set.Set Pos, [(Int, [Pos])], [(Int, Int)], Int, Int)

stepCave :: CaveState -> CaveState
stepCave (cave, (rIx, r) : rs, (_, j) : js, nr, h)
  | downR /= sideR = (cave, (rIx, downR) : rs, js, nr, h)
  | otherwise =
      let newH = maximum $ h : map fst sideR
          spawnNext = rs & ix 0 . _2 %~ map (_1 +~ newH)
       in (foldr Set.insert cave sideR, spawnNext, js, nr + 1, newH)
  where
    [downR, sideR, _] = scanr ($) r [tryMove (_1 -~ 1), tryMove (_2 +~ j)]
    tryMove = (fromMaybe <*>) . mapM . (((<$) <*> guard . isValid) .)
    isValid p@(_, y) = p `Set.notMember` cave && y >= 0 && y < 7

findCycle :: Int -> [(Int, Int)] -> (CaveState, Int, Int, Int, Int)
findCycle depth js = findCycle' Map.empty (Set.fromAscList $ map (0,) [0 .. 6], rocks, js, 0, 0)
  where
    findCycle' cache state@(cave, (rIx, r) : rs, js@((jIx, _) : _), nr, h)
      | rIx /= jIx = findCycle' cache $ stepCave state
      | Just (nr1, h1) <- Map.lookup key cache = (contState, nr1, h1, nr - nr1, h - h1)
      | otherwise = findCycle' (Map.insert key (nr, h) cache) $ stepCave state
      where
        contState = (stackTop, normR : rs, js, 0, 0)
        key@(normR, _) = ((rIx, map (_1 -~ h) r), stackTop)
        stackTop = Set.dropWhileAntitone ((< -depth) . fst) $ Set.map (_1 -~ h) cave

stackHeight :: Int -> (CaveState, Int, Int, Int, Int) -> Int
stackHeight n (contState, nr, h, dnr, dh) =
  let (cycleN, rRem) = (n - nr) `divMod` dnr
      Just state = find ((>= rRem) . (^. _4)) $ iterate stepCave contState
   in h + cycleN * dh + state ^. _5

main :: IO ()
main = do
  cycleInfo <- findCycle 100 . parse <$> getContents
  print $ stackHeight 2_022 cycleInfo
  print $ stackHeight 1_000_000_000_000 cycleInfo
  where
    parse = cycle . zip [0 ..] . map (\d -> if d == '<' then -1 else 1) . init
