{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Lens hiding (re)
import Data.List.Split (splitOn)
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Set qualified as Set
import Text.Regex.PCRE.Heavy (re, scan)

type ValveGraph = Map String (Int, [String])

type DistanceMap = Map String (Map String Int)

allDists :: ValveGraph -> DistanceMap
allDists vs = Map.mapWithKey (\v _ -> Map.fromList $ minDists [(v, 0)] Set.empty []) vs
  where
    minDists [] _ dists = dists
    minDists (d@(src, dist) : nexts) seen dists =
      let adjs' = [(n, dist + 1) | n <- snd $ vs ! src, n `notElem` seen]
          seen' = foldr Set.insert seen $ src : map fst adjs'
       in minDists (nexts ++ adjs') seen' $ d : dists

maxDrain :: ValveGraph -> DistanceMap -> [(String, Int)] -> Int
maxDrain vs dm ((src, t) : ws) = maximum $ Map.mapWithKey (potentialDrain src) vs
  where
    potentialDrain _ _ (0, _) = 0
    potentialDrain src v (r, _) =
      let t' = t - dm ! src ! v - 1
       in if t' < 0 then 0 else r * t' + maxDrain (vs & ix v . _1 .~ 0) dm (ws ++ [(v, t')])

main :: IO ()
main = do
  (valves, dists) <- ((,) <*> allDists) . Map.fromList . map parseValve . lines <$> getContents
  print $ maxDrain valves dists [("AA", 30)]
  print $ maxDrain valves dists [("AA", 26), ("AA", 26)]
  where
    parseValve = matchToValve . snd . head . scan [re|([A-Z]{2})[^\d]+(\d+).+es? (.+)$|]
    matchToValve [src, rate, dests] = (src, (read rate, splitOn ", " dests))
