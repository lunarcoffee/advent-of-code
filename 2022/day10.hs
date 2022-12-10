import Control.Monad
import Data.List (scanl')
import Data.List.Split (chunksOf, splitOn)

regStates :: [Int -> Int] -> [Int]
regStates = init . scanl' (flip ($)) 1

sumStrengths :: [Int -> Int] -> Int
sumStrengths instrs = sum $ map ((*) <*> ((0 : regStates instrs) !!)) [20, 60 .. 220]

generateCRT :: [Int] -> [String]
generateCRT = chunksOf 40 . zipWith cellValue [0 ..]
  where
    cellValue pixel sprite
      | abs (pixel `mod` 40 - sprite) < 2 = '#'
      | otherwise = ' '

main :: IO ()
main = do
  input <- parse <$> getContents
  print $ sumStrengths input
  mapM_ putStrLn $ generateCRT $ regStates input
  where
    parseInstruction ["addx", v] = [id, (+ read v)]
    parseInstruction _ = [id]
    parse = parseInstruction . splitOn " " <=< lines
