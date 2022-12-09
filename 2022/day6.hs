import Data.List (nub, tails)

prefixLength :: Int -> String -> Int
prefixLength n = length . takeWhile ((/=) <*> nub) . map (take n) . tails

main :: IO ()
main = do
  input <- getContents
  print $ prefixLength 4 input + 4
  print $ prefixLength 14 input + 14
