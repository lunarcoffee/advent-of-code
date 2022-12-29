import Data.List (elemIndices, foldl1')

fromSnafu :: String -> Int
fromSnafu = foldl1' ((+) . (5 *)) . map (2 -) . ((`elemIndices` "210-=") =<<)

toSnafu :: Int -> String
toSnafu 0 = ""
toSnafu n = let (q, r) = (n + 2) `divMod` 5 in toSnafu q ++ ["=-012" !! r]

main :: IO ()
main = do
  fuels <- map fromSnafu . lines <$> getContents
  putStrLn $ toSnafu $ sum fuels
