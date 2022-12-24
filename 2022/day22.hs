{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

import Control.Lens hiding (re)
import Control.Monad
import Data.List (elemIndex, elemIndices, foldl', scanl')
import Data.Maybe (isNothing)
import Data.Vector (Vector, (!?))
import Data.Vector qualified as Vec
import Text.Regex.PCRE.Heavy (re, scan)

type Pos = (Int, Int)

type Field = Vector (Vector (Maybe Char))

(@) :: Field -> Pos -> Maybe Char
cs @ (x, y) = join $ cs !? x >>= (!? y)

runMove :: Field -> (Pos -> Pos) -> Pos -> Pos
runMove cs m p@((cs @) . m -> Just c) = if c == '#' then p else m p
runMove cs m p = runMove cs (wrap . m) p
  where
    wrap = head . dropWhile (isNothing . (cs @)) . iterate (join bimap (`mod` maxDim) . m)
    maxDim = max (Vec.length cs) $ Vec.maximum $ Vec.length <$> cs

flatPassword :: Field -> [Pos -> Pos] -> Int
flatPassword cs ms =
  let Just start = Vec.elemIndex (Just '.') $ Vec.head cs
      (row, col) = foldl' (flip $ runMove cs) (0, start) ms
      Just facing = elemIndex (last ms (0, 0)) [(0, 1), (1, 0), (0, -1), (-1, 0)]
   in 1_000 * (row + 1) + 4 * (col + 1) + facing

main :: IO ()
main = do
  (cells, moves) <- bimap parseMap (parseMoves . last) . span (/= "") . lines <$> getContents
  print $ flatPassword cells moves
  where
    parseMoves = fst <=< scanl' parsePair ([], 0) . map snd . scan [re|(\d+)(\w)?|]
    parsePair s [n] = parsePair s [n, "F"]
    parsePair (_, d) [n, [dir]] =
      let dirs = flip (+~) <$> [1, -1] <*> [_2, _1]
       in (replicate (read n) (dirs !! d), (d + head (elemIndices dir "LFR") - 1) `mod` 4)
    parseMap = Vec.fromList . map (Vec.fromList . map parseCell)
    parseCell c = c <$ guard (c /= ' ')
