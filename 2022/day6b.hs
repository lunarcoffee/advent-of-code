import Data.List (nub, tails)

main :: IO ()
main = getContents >>= print . (+ 14) . prefixLength
  where
    prefixLength = length . takeWhile ((/=) <*> nub) . map (take 14) . tails
