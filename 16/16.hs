import Data.List
import Data.Maybe

input = "00101000101111010"

dragonCurve :: String -> String
dragonCurve a = a ++ (dragonCurve' a)
  where
    dragonCurve' :: String -> String
    dragonCurve' b = let n = '0' : map bitFlip (reverse b)
                     in (n ++ (dragonCurve' (b ++ n)))

bitFlip '0' = '1'
bitFlip '1' = '0'

checksum :: String -> String
checksum = fromJust . find (odd . length) . iterate checksum'
  where
    checksum' (a:b:xs) | a == b = '1' : checksum' xs
                       | a /= b = '0' : checksum' xs
    checksum' [] = []                  

main = do
  print $ checksum $ take 272 $ dragonCurve input
  print $ checksum $ take 35651584 $ dragonCurve input
