{-# LANGUAGE ViewPatterns #-}

import Control.Lens
import Data.Function (on)
import Data.Map (Map, (!))
import Data.Map qualified as Map
import Data.Ratio (numerator)

data Expr = Value Rational | Expr String String (Rational -> Rational -> Rational)

calculate :: Map String Expr -> String -> Rational
calculate vs v = case vs ! v of
  Value x -> x
  Expr a b op -> (op `on` calculate vs) a b

humanValue :: Map String Expr -> Integer
humanValue vs = let (a, b) = coeffs "root" in numerator $ -b / a
  where
    coeffs ((vs !) -> Expr a b op) = if hasHuman a then wrap a b $ flip op else wrap b a op
    coeffs "humn" = (1, 0)
    wrap a b op =
      let opIx = fromInteger $ numerator $ op 1 1
          (fx, fy) = let op' = op $ calculate vs b in ([negate, op', id] !! opIx, op')
       in bimap fx fy $ coeffs a
    hasHuman ((vs !) -> Expr a b _) = any hasHuman [a, b]
    hasHuman v = v == "humn"

main :: IO ()
main = do
  vars <- Map.fromList . map parseLine . lines <$> getContents
  print $ numerator $ calculate vars "root"
  print $ humanValue $ vars & ix "root" %~ \(Expr a b _) -> Expr a b (-)
  where
    parseLine = bimap (take 4) (parseExpr . words) . splitAt 6
    parseExpr [a, op, b] =
      let Just opFn = lookup op [("+", (+)), ("-", (-)), ("*", (*)), ("/", (/))]
       in Expr a b opFn
    parseExpr [n] = Value $ fromInteger $ read n
