import Data.List (foldl1')
import Data.List.Split (splitOn)
import Data.Map qualified as M

type Game = M.Map Char Int

parseGame :: String -> Game
parseGame = foldl1' (M.unionWith max) . map parseRound . splitOn "; " . last . splitOn ": "
  where
    parseRound = M.fromList . map ((\[n, c : _] -> (c, read n)) . words) . splitOn ", "

sumValidGameIds :: [Game] -> Int
sumValidGameIds = sum . map fst . filter (isGameValid . snd) . zip [1 ..]
  where
    isGameValid r = and $ zipWith (<=) (map (r !) "rgb") [12 ..]
    (!) = flip $ M.findWithDefault 0

sumGamePowers :: [Game] -> Int
sumGamePowers = sum . map (product . M.elems)

main :: IO ()
main = do
  games <- map parseGame . lines <$> getContents
  print $ sumValidGameIds games
  print $ sumGamePowers games
