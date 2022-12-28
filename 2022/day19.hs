import Control.Lens
import Data.Char (isDigit)
import Data.Group (Group (..))
import Data.List (elemIndex)
import Data.List.Split (wordsBy)
import Data.Maybe (fromJust)
import Data.Semigroup (Semigroup (..), Sum (..))
import Data.Vector.Unboxed (Vector, (!))
import Data.Vector.Unboxed qualified as Vec

type Materials = (Sum Int, Sum Int, Sum Int, Sum Int)

type Blueprint = ([(Materials, Materials)], Materials)

runBlueprint :: [(Materials, Materials, Int)] -> Vector Int -> Blueprint -> Int
runBlueprint [] maxAt _ = Vec.head maxAt
runBlueprint ((inv@(_, _, _, Sum g), gain@(_, _, _, Sum g'), t) : next) maxAt p@(bp, maxes)
  | t <= 0 || potentialTooLow = runBlueprint next maxAt' p
  | otherwise = runBlueprint (map withRobot (filter shouldMake bp) ++ next) maxAt' p
  where
    potentialTooLow = or $ zipWith ((>) . (maxAt !)) <*> map potentialAt $ [0 .. t]
    potentialAt t' = g + sum [g' .. g' + t - t']
    maxAt' = maxAt & ix t %~ max g
    withRobot (cost, gain')
      | t < deltaT = (inv <> t `stimes` gain, gain, 0)
      | otherwise = (inv <> deltaT `stimes` gain ~~ cost, gain <> gain', t - deltaT)
      where
        deltaT = 1 + maximum (0 : map neededT (snd $ robotValues cost))
        neededT [Sum cost, Sum cur, Sum rate, _] = (cost - cur - 1) `div` rate + 1
    shouldMake (cost, _) =
      let ([_, _, rate, effMax], costs) = robotValues cost
       in rate < effMax && all ((> 0) . (!! 2)) costs
    robotValues cost =
      let robotData = [(_1, [_1]), (_2, [_1]), (_3, [_1, _2]), (_4, [_1, _3])]
          values i = map (^. i) [cost, inv, gain, maxes]
       in (bimap <*> map) values $ robotData !! fromJust (elemIndex cost $ map fst bp)

maxGeodes :: Int -> Blueprint -> Int
maxGeodes n = runBlueprint [((0, 0, 0, 0), (1, 0, 0, 0), n)] $ Vec.replicate (n + 1) 0

main :: IO ()
main = do
  blueprints <- map (parseBlueprint . parseCosts) . lines <$> getContents
  print $ sum $ zipWith ((*) . maxGeodes 24) blueprints [1 ..]
  print $ product $ map (maxGeodes 32) $ take 3 blueprints
  where
    parseBlueprint [o, c, oo, oc, go, gb] =
      let costs = [(o, 0, 0, 0), (c, 0, 0, 0), (oo, oc, 0, 0), (go, 0, gb, 0)]
          gains = map (((0, 0, 0, 0) &) . (.~ 1)) [_1, _2, _3, _4]
       in (zip costs gains, (maximum [o, c, oo, go], oc, gb, maxBound))
    parseCosts = tail . map (Sum . read) . wordsBy (not . isDigit)
