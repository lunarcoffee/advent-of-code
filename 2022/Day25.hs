import Control.Monad
import Data.Foldable (toList)
import Data.List (elemIndices, foldl1')
import Data.Sequence (unfoldl)

toSnafu :: Int -> Maybe (Int, Char)
toSnafu n = ("=-012" !!) <$> (n + 2) `divMod` 5 <$ guard (n > 0)

main :: IO ()
main = do
  fuels <- map parseSnafu . lines <$> getContents
  putStrLn $ toList $ unfoldl toSnafu $ sum fuels
  where
    parseSnafu = foldl1' ((+) . (5 *)) . map (2 -) . ((`elemIndices` "210-=") =<<)
