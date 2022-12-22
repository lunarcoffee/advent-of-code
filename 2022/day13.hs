{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad
import Data.List (findIndices, sort)
import Data.List.Split (chunksOf, splitOn)
import Text.Regex.PCRE.Heavy (gsub, re)

data Packet = Int Int | List [Packet] deriving (Eq, Read)

instance Ord Packet where
  compare (Int a) (Int b) = compare a b
  compare (List a) (List b) = compare a b
  compare a@(Int _) b = compare (List [a]) b
  compare a b = compare a $ List [b]

orderedIndexSum :: [Packet] -> Int
orderedIndexSum = sum . map (+ 1) . findIndices (\[x, y] -> x < y) . chunksOf 2

decoderKey :: [Packet] -> Int
decoderKey =
  let dividers = map (\x -> List [List [Int x]]) [2, 6]
   in product . map (+ 1) . findIndices (`elem` dividers) . sort . (dividers ++)

main :: IO ()
main = do
  packets <- (map parsePacket . lines <=< splitOn "\n\n") <$> getContents
  print $ orderedIndexSum packets
  print $ decoderKey packets
  where
    parsePacket = read . prepend [re|(\d+)|] "Int " . prepend [re|(\[)|] "List "
    prepend regex prefix = gsub regex $ (prefix ++) . head
