import Data.List (nub, tails)

main :: IO ()
main = getContents >>= print . (+ 4) . prefixLength
  where
    prefixLength = length . takeWhile ((/=) <*> nub) . map (take 4) . tails
