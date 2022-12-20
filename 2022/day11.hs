import Control.Lens
import Data.List (foldl', sort)
import Data.List.Split (splitOn)

type Monkey = ([Integer], Integer -> Integer, Integer, Int, Int, Int)

playRound :: (Integer -> Integer) -> [Monkey] -> [Monkey]
playRound reduceWorry ms = foldl' playMonkey ms [0 .. length ms - 1]
  where
    playMonkey ms m = foldl' processItem (ms & ix m . _6 +~ length inv & ix m . _1 .~ []) inv
      where
        (inv, op, md, t, f, _) = ms !! m
        processItem ms item =
          let newItem = reduceWorry (op item) `mod` allMod
           in ms & ix (if newItem `mod` md == 0 then t else f) . _1 %~ (++ [newItem])
    allMod = product $ map (^. _3) ms

monkeyBusiness :: [Monkey] -> Integer
monkeyBusiness = product . take 2 . reverse . sort . map (toInteger . (^. _6))

main :: IO ()
main = do
  monkeys <- parse <$> getContents
  print $ monkeyBusiness $ iterate (playRound (`div` 3)) monkeys !! 20
  print $ monkeyBusiness $ iterate (playRound id) monkeys !! 10_000
  where
    parseOp "* old" = (^ 2)
    parseOp (op : _ : n) = (if op == '+' then (+) else (*)) $ read n
    parseMonkey [inv, op, md, t, f] =
      let r n = read . drop n
       in (read $ '[' : drop 18 inv ++ "]", parseOp $ drop 23 op, r 21 md, r 29 t, r 30 f, 0)
    parse = map (parseMonkey . tail . lines) . splitOn "\n\n"
