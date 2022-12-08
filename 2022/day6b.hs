import Data.List (nub, tails)

prefixLength :: String -> Int
prefixLength = length . takeWhile ((/=) <*> nub) . map (take 14) . tails

main :: IO ()
main = getContents >>= print . (+ 14) . prefixLength
