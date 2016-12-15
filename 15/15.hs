import Control.Arrow (second)
import Control.Monad (liftM, liftM2)
import Data.List (find, findIndices)
import Data.Monoid ((<>))
import Text.Parsec

parseInput = endBy parseLine (char '\n')

parseLine = liftM2 (,) (string "Disc #" *> digit *> string " has " *> number)
                       (string " positions; at time=0, it is at position "
                        *> number <* char '.')
number = liftM (read :: String -> Int) (many digit)

shiftInput = zipWith (\c -> \(a,b) -> (a,(10*a+b+c) `mod` a)) [1..]

stupidSearch :: [(Int,Int)] -> [Int]
stupidSearch xs = findIndices isValid [map (\(a,b) -> (b + c) `mod` a) xs | c <- [0..]]

isValid :: [Int] -> Bool
isValid = all (==0)

main = do
  input <- getContents
  let parsed = parse parseInput "error" input
  print $ head <$> stupidSearch <$> shiftInput <$> parsed
  print $ head <$> stupidSearch <$> shiftInput <$> (<> [(11,0)]) <$> parsed
