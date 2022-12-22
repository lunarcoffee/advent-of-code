import Control.Monad
import Data.List (nub, tails)

prefixLength :: Int -> String -> Int
prefixLength n = (+ n) . length . takeWhile ((/=) <*> nub) . map (take n) . tails

main :: IO ()
main = do
  signal <- getContents
  print $ prefixLength 4 signal
  print $ prefixLength 14 signal
