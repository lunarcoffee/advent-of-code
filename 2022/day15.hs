{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

import Data.List (find, foldl', sort)
import Text.Regex.PCRE.Heavy (re, scan)

type Pos = (Int, Int)

noBeaconRanges :: Int -> [(Pos, Pos)] -> [(Int, Int)]
noBeaconRanges y = (foldl' merge =<< (: []) . head) . sort . filter (uncurry (<=)) . map toRange
  where
    toRange ((a, b), (c, d)) = let x = abs (c - a) + abs (d - b) - abs (y - b) in (a - x, a + x)
    merge ar@((a, b) : rs) r@(c, d)
      | c <= b = rs ++ [(a, max b d)]
      | otherwise = ar ++ [r]

distressTuningFreq :: [(Pos, Pos)] -> Int
distressTuningFreq bs =
  let ixedRanges = (zip <*> map (`noBeaconRanges` bs)) [0 .. 4_000_000]
      Just (y, (_, x) : _) = find ((> 1) . length . snd) ixedRanges
   in 4_000_000 * (x + 1) + y

main :: IO ()
main = do
  beacons <- parse <$> getContents
  print $ sum $ map (uncurry subtract) $ noBeaconRanges 2_000_000 beacons
  print $ distressTuningFreq beacons
  where
    parseBeacon = (\[a, b, x, y] -> ((a, b), (x, y))) . map (read . fst) . scan [re|-?\d+|]
    parse = map parseBeacon . lines
