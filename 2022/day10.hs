import Control.Monad
import Data.List (scanl')
import Data.List.Split (chunksOf, splitOn)

regStates :: [Int -> Int] -> [Int]
regStates = init . scanl' (flip ($)) 1

sumStrengths :: [Int] -> Int
sumStrengths sprites = sum $ map ((*) <*> (sprites !!) . pred) [20, 60 .. 220]

generateCRT :: [Int] -> [String]
generateCRT = chunksOf 40 . zipWith cellValue [0 ..]
  where
    cellValue pixel sprite
      | abs (pixel `mod` 40 - sprite) < 2 = '#'
      | otherwise = ' '

main :: IO ()
main = do
  states <- regStates . parse <$> getContents
  print $ sumStrengths states
  mapM_ putStrLn $ generateCRT states
  where
    parseInstruction ["addx", v] = [id, (+ read v)]
    parseInstruction _ = [id]
    parse = parseInstruction . splitOn " " <=< lines
