{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Lens hiding (re)
import Data.List.Split (splitOn)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Text.Regex.PCRE.Heavy (re, scan)

type ValveGraph = Map.Map String (Int, [String])

type DistanceMap = Map.Map String (Map.Map String Int)

allDists :: ValveGraph -> DistanceMap
allDists vs = Map.mapWithKey (\v _ -> Map.fromList $ minDists [(v, 0)] Set.empty []) vs
  where
    minDists [] seen dists = dists
    minDists (d@(src, dist) : nexts) seen dists =
      let newAdjs = [(n, dist + 1) | n <- snd $ vs Map.! src, n `notElem` seen]
          newSeen = foldr Set.insert seen $ src : map fst newAdjs
       in minDists (nexts ++ newAdjs) newSeen $ d : dists

maxDrain :: ValveGraph -> DistanceMap -> [(String, Int)] -> Int -> Int
maxDrain vs dm workers@((src, doneT) : ws) t = maximum $ Map.mapWithKey (potentialDrain src) vs
  where
    potentialDrain _ _ (0, _) = 0
    potentialDrain src v (rate, _) =
      let newT = t - dm Map.! src Map.! v - 1
          newWs@((_, nextT) : _) = ws ++ [(v, newT)]
       in if newT < 0 then 0 else rate * newT + maxDrain (vs & ix v . _1 .~ 0) dm newWs nextT

main :: IO ()
main = do
  (valves, dists) <- ((,) <*> allDists) . parse <$> getContents
  print $ maxDrain valves dists [("AA", 30)] 30
  print $ maxDrain valves dists [("AA", 26), ("AA", 26)] 26
  where
    matchToValve [src, rate, dests] = (src, (read rate, splitOn ", " dests))
    parseValve = matchToValve . snd . head . scan [re|([A-Z]{2})[^\d]+(\d+).+es? (.+)$|]
    parse = Map.fromList . map parseValve . lines
