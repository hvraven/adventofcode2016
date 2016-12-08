import Control.Lens
import Control.Monad
import Data.Either
import Data.List
import Data.Tuple
import Text.Parsec

data Command = Rect Int Int
             | RotateCol Int Int
             | RotateRow Int Int
             deriving (Show)

rotate :: Int -> [a] -> [a]
rotate n xs = uncurry (++) $ swap $ splitAt (length xs - n) xs

rotateRow :: Int -> Int -> [[a]] -> [[a]]
rotateRow p n xs = xs & element p %~ (rotate n)
rotateColumn p n = transpose . rotateRow p n . transpose

rectangle a b xs = xs & (elements (<b) . elements (<a)) .~ True

display = replicate 6 $ replicate 50 False

parseInput = sepBy parseLine (char '\n') <* eof

parseLine = try parseRect <|> try parseRotateRow <|> parseRotateCol

parseRect = liftM2 Rect (string "rect " *> int) (char 'x' *> int)

int = read <$> (many digit)

parseRotateRow = liftM2 RotateRow (string "rotate row y=" *> int)
                                  (string " by " *> int)

parseRotateCol = liftM2 RotateCol (string "rotate column x=" *> int)
                                  (string " by " *> int)

process :: [[Bool]] -> Command -> [[Bool]]
process xs (Rect a b)      = rectangle a b xs
process xs (RotateRow p n) = rotateRow p n xs
process xs (RotateCol p n) = rotateColumn p n xs

printDisplay = putStrLn . unlines . map (map pixel)
  where
    pixel True  = 'x'
    pixel False = ' '

main = do
  input <- getContents
  let commands = concat $ rights [parse parseInput "Error" input]
  foldM (\d -> \c -> do let d' = process d c
                        print c
                        printDisplay d'
                        return d') display commands
  print $ sum $ map (length . filter id) $ foldl process display commands