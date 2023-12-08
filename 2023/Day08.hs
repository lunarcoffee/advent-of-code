{-# LANGUAGE LambdaCase #-}

import Data.Function ((&))
import Data.List (foldl1', genericLength)
import Data.Map qualified as M

-- Maps each node to its two neighbors
type Graph = M.Map String (String, String)

-- A sequence of functions which select one of the two neighbors (e.g. `fst`, `snd`)
type Moves = [(String, String) -> String]

parseNodes :: [String] -> Graph
parseNodes = M.fromList . map (parseLine . words)
  where
    parseLine [from, _, _ : l, r] = (from, (init l, init r))

-- The minimum number of steps required until all sources (the `[String]`) reach an end node
-- simultaneously when following `moves` with `g`
--
-- With a little experimenting, I figured out that (at least in my input), it's always true that:
--
-- 1) Once the first end node is reached, there is another end node after it
-- 2) The next end node after the first is the same node (and hence all subsequent end nodes)
-- 3) The number of steps until the first end node is the same as between it and the next
--
-- So, each path starting from a given `src` looks something like this:
--
-- src -> ... (n nodes) ... -> XYZ -> ... (n nodes) ... -> XYZ -> ...
--
-- If we can find `n` for each `src`, the LCM of all the `n` will be the number we want, since a
-- sequence is at an end node only when the number of steps taken is a multiple of `n`.
minSteps :: Graph -> Moves -> [String] -> Integer
minSteps g moves = foldl1' lcm . map cycleLength
  where
    -- This really just finds the distance until the first end node (see comment above)
    cycleLength src =
      let path = scanl ((&) . (g M.!)) src $ cycle moves
       in genericLength $ takeWhile ((/= 'Z') . last) path

main :: IO ()
main = do
  moveStr : _ : nodes <- lines <$> getContents
  let moves = map (\case 'L' -> fst; 'R' -> snd) moveStr
  let g = parseNodes nodes

  print $ minSteps g moves ["AAA"]
  print $ minSteps g moves $ filter ((== 'A') . last) $ M.keys g
