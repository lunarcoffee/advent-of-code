import Data.List (nub, tails)

prefixLength :: String -> Int
prefixLength = length . takeWhile ((/=) <*> nub) . map (take 4) . tails

main :: IO ()
main = getContents >>= print . (+ 4) . prefixLength
