import Data.List.Split (splitOn)

main :: IO ()
main = getContents >>= print . maximum . map sum . parseInvs
  where
    parseInvs = map (map read . lines) . splitOn "\n\n"
