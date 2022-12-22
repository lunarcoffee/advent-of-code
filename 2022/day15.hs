{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Parallel.Strategies (parMap, rpar)
import Data.Foldable (asum)
import Data.List (find, foldl', sort)
import GHC.Conc (numCapabilities)
import Text.Regex.PCRE.Heavy (re, scan)

type Pos = (Int, Int)

noBeaconRanges :: Int -> [(Pos, Pos)] -> [(Int, Int)]
noBeaconRanges y = (foldl' merge =<< (: []) . head) . sort . filter (uncurry (<=)) . map toRange
  where
    toRange ((a, b), (c, d)) = let x = abs (c - a) + abs (d - b) - abs (y - b) in (a - x, a + x)
    merge ar@((a, b) : rs) r@(c, d)
      | c <= b + 1 = (a, max b d) : rs
      | otherwise = r : ar

distressTuningFreq :: Int -> Int -> [(Pos, Pos)] -> Int
distressTuningFreq from to bs =
  let chunkSize = (to - from + 1) `div` numCapabilities * 4
      chunks = [[c .. c + chunkSize - 1] | c <- [from, from + chunkSize .. to]]
      Just (y, [_, (_, x)]) = asum $ parMap rpar findBeacon chunks
   in to * (x + 1) + y
  where
    findBeacon = find ((> 1) . length . snd) . (zip <*> map (`noBeaconRanges` bs))

main :: IO ()
main = do
  beacons <- map parseBeacon . lines <$> getContents
  print $ sum $ map (uncurry subtract) $ noBeaconRanges 2_000_000 beacons
  print $ distressTuningFreq 0 4_000_000 beacons
  where
    parseBeacon = (\[a, b, x, y] -> ((a, b), (x, y))) . map (read . fst) . scan [re|-?\d+|]
