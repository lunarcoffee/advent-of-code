import Data.List (nub, tails)

prefixLength :: Int -> String -> Int
prefixLength n = length . takeWhile ((/=) <*> nub) . map (take n) . tails

main :: IO ()
main = do
  signal <- getContents
  print $ prefixLength 4 signal + 4
  print $ prefixLength 14 signal + 14
