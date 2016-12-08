import Control.Monad
import Control.Arrow
import Data.Char
import Data.List
import Text.Parsec
import Data.Either

data Room = Room { name :: String
                 , sectorID :: Int
                 , checksum :: String
                 } deriving (Show)

main = do
  input <- getContents
  let rooms = concat $ rights $ [(parse parseRooms "Error") input]
  print $ map (id &&& calcChecksum) rooms
  print $ sum $ map sectorID $ filter verifyChecksum rooms
  print $ map (id &&& decrypt) $ filter (\r -> "north" `isInfixOf` (decrypt r))
                               $ filter verifyChecksum rooms


parseRoom = liftM3 (Room) (init <$> (many1 (letter <|> char '-')))
                          (read <$> (many1 digit))
                          (char '[' *> many1 lower <* char ']')

parseRooms = parseRoom `endBy` char '\n' <* eof

verifyChecksum :: Room -> Bool
verifyChecksum r = checksum r == calcChecksum r

decrypt :: Room -> String
decrypt r = map (rotate' (sectorID r)) (name r)
  where
    rotate' _ '-' = ' '
    rotate' _ ' ' = ' '
    rotate' i c   = chr $ ord 'a' + ((ord c - ord 'a' + i) `mod` 26)

calcChecksum = take 5 . map snd . sort . map (((-) 1000 . length) &&& head) . group . sort . filter (/= '-') . name