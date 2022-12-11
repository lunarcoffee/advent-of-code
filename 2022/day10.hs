import Control.Monad
import Data.List (scanl')
import Data.List.Split (chunksOf, splitOn)

sumStrengths :: [Int] -> Int
sumStrengths states = sum [c * states !! (c - 1) | c <- [20, 60 .. 220]]

generateCRT :: [Int] -> [String]
generateCRT = chunksOf 40 . zipWith cellValue [0 ..]
  where
    cellValue pixel sprite
      | abs (pixel `mod` 40 - sprite) < 2 = '#'
      | otherwise = ' '

main :: IO ()
main = do
  states <- init . scanl' (+) 1 . parseToStates <$> getContents
  print $ sumStrengths states
  mapM_ putStrLn $ generateCRT states
  where
    parseInstruction (_ : v) = 0 : map read v
    parseToStates = parseInstruction . splitOn " " <=< lines
