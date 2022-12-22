import Control.Lens
import Data.Foldable (foldl', toList)
import Data.Sequence qualified as Seq

mix :: Seq.Seq (Int, Int) -> Seq.Seq (Int, Int)
mix xs = foldl' shift xs [0 .. length xs - 1]
  where
    shift xs nth =
      let Just i = Seq.findIndexL ((== nth) . fst) xs
          x@(_, delta) = xs `Seq.index` i
       in Seq.insertAt ((i + delta) `mod` (Seq.length xs - 1)) x $ Seq.deleteAt i xs

groveCoordSum :: Seq.Seq (Int, Int) -> Int
groveCoordSum xs =
  let afterZero = dropWhile (/= 0) $ cycle $ toList $ snd <$> xs
   in sum $ map (afterZero !!) [1_000, 2_000, 3_000]

main :: IO ()
main = do
  file <- parseIndexed <$> getContents
  print $ groveCoordSum $ mix file
  print $ groveCoordSum $ iterate mix ((_2 *~ 811_589_153) <$> file) !! 10
  where
    parseIndexed = Seq.fromList . zip [0 ..] . map read . lines
