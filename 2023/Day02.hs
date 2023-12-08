import Data.List (foldl1')
import Data.List.Split (splitOn)
import Data.Map qualified as M

-- Something like {'b': 7, 'g': 15, 'r': 10}, a map which counts the maximum frequency of each
-- color of ball drawn over all rounds in a game, not including colors that weren't drawn
type Game = M.Map Char Int

-- `M.unionWith max` merges two maps (representing rounds here) together, picking the maximum value
-- (frequency) for any keys (colors) in common
parseGame :: String -> Game
parseGame = foldl1' (M.unionWith max) . map parseRound . splitOn "; " . last . splitOn ": "
  where
    -- Turns something like "5 blue, 2 red" into {'b': 5, 'r': 2}
    parseRound = M.fromList . map ((\[n, c : _] -> (c, read n)) . words) . splitOn ", "

-- Associate each game with its ID (its 1-based index), find the valid ones, and sum their IDs
sumValidGameIds :: [Game] -> Int
sumValidGameIds = sum . map fst . filter (isGameValid . snd) . zip [1 ..]
  where
    -- Like `g ! 'r' <= 12 && g ! 'g' <= 13 && g ! 'b' <= 14` but more general (e.g. we can easily
    -- change the operator or add more colors)
    --
    -- `zipWith f [a, b, c] [w, x, y, z]` gives the same result as `[f a w, f b x, f c y]`, kind of
    -- like mapping over two lists. Also note that `zip` is the same as `zipWith (,)`, and that
    -- `(<=) . (g !)` is equivalent to `\color max -> g ! color <= max`
    isGameValid g = and $ zipWith ((<=) . (g !)) "rgb" [12 ..]
    (!) = flip $ M.findWithDefault 0

sumGamePowers :: [Game] -> Int
sumGamePowers = sum . map (product . M.elems)

main :: IO ()
main = do
  games <- map parseGame . lines <$> getContents
  print $ sumValidGameIds games
  print $ sumGamePowers games
