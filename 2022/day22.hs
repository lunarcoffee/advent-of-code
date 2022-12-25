{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Applicative
import Control.Lens hiding (re)
import Control.Monad
import Data.List (elemIndex, find, foldl', scanl')
import Data.Maybe (fromJust, isJust)
import Data.Vector (Vector, (!?))
import Data.Vector qualified as Vec
import Text.Regex.PCRE.Heavy (re, scan)

type Pos = (Int, Int)

type Field = Vector (Vector (Maybe Char))

(@) :: Field -> Pos -> Maybe Char
cs @ (x, y) = join $ cs !? x >>= (!? y)

fromFacing :: Int -> Pos -> Pos
fromFacing = ((flip (+~) <$> [1, -1] <*> [_2, _1]) !!)

toFacing :: (Pos -> Pos) -> Maybe Int
toFacing m = elemIndex (m (0, 0)) [(0, 1), (1, 0), (0, -1), (-1, 0)]

type Wrapping = Field -> (Pos -> Pos) -> Pos -> (Pos, Int)

stepMove :: Field -> Wrapping -> (Pos, Int) -> (Pos -> Pos) -> (Pos, Int)
stepMove cs wrap s@(p, mo) m
  | Just '#' <- cs @ m' p <|> cs @ p' = s
  | Just _ <- cs @ m' p = s & _1 %~ m'
  | otherwise = (p', (mo + mo') `mod` 4)
  where
    (p', mo') = wrap cs m' p
    m' = maybe m (fromFacing . (`mod` 4) . (+ mo)) $ toFacing m

wrapSide, wrapCube :: Wrapping
wrapSide cs m = (,0) . fromJust . find (isJust . (cs @)) . iterate wrapMove . m
  where
    wrapMove = (both %~ (`mod` 200)) . m
wrapCube _ m p@(r, c)
  | fst << 49 = ifFacing 0 ((149 - r, 99), 2) ((149 - r, 0), 2)
  | fst << 99 = ifFacing 0 ((49, r + 50), 3) ((100, r - 50), 3)
  | fst << 149 = ifFacing 0 ((149 - r, 149), 2) ((149 - r, 50), 2)
  | fst << 199 = ifFacing 0 ((149, r - 100), 3) ((0, r - 100), 3)
  | snd << 49 = ifFacing 1 ((0, c + 100), 0) ((c + 50, 50), 1)
  | snd << 99 = ifFacing 1 ((c + 100, 49), 1) ((c + 100, 0), 1)
  | otherwise = ifFacing 1 ((c - 50, 99), 1) ((199, c - 100), 0)
  where
    ifFacing f a b = if toFacing m == Just f then a else b
    dim << top = all ((`elem` [top - 49 .. top]) . dim) [p, m p]

findPassword :: Field -> [Pos -> Pos] -> Wrapping -> Int
findPassword cs ms wrap =
  let Just start = Vec.elemIndex (Just '.') $ Vec.head cs
      ((row, col), mo) = foldl' (stepMove cs wrap) ((0, start), 0) ms
      facing = (fromJust (toFacing $ last ms) + mo) `mod` 4
   in 1_000 * (row + 1) + 4 * (col + 1) + facing

main :: IO ()
main = do
  (cells, moves) <- bimap parseMap (parseMoves . last) . span (/= "") . lines <$> getContents
  print $ findPassword cells moves wrapSide
  print $ findPassword cells moves wrapCube
  where
    parseMap = Vec.fromList . map (Vec.fromList . map ((<$) <*> guard . (/= ' ')))
    parseMoves = fst <=< scanl' parsePair ([], 0) . map snd . scan [re|(\d+)(\w)?|]
    parsePair s [n] = parsePair s [n, "F"]
    parsePair (_, d) [n, [dir]] = (replicate (read n) $ fromFacing d, (d + parseDir dir) `mod` 4)
    parseDir = pred . fromJust . (`elemIndex` "LFR")
