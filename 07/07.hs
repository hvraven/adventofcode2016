import Text.Parsec
import Control.Monad
import Text.Regex.PCRE

parseInput = parseIp `sepBy` char '\n'

parseIp = do
  start <- many letter
  (outer,inner) <- liftM unzip $ many parseIp'
  return (outer, start : inner)

parseIp' = liftM2 (,) (char '[' *> many letter <* char ']') (many letter)

hasABBA :: String -> Bool
hasABBA a = a =~ "(.)([^\\1])\\2\\1" && (not (a =~ "(.)\\1\\1\\1"))

useTLS (inner,outer) = any hasABBA outer && (all (not . hasABBA) inner)

findABA :: String -> [(Char,Char)]
findABA s = fmap (reformat . tail) (s =~ "(.)(?=(.)\\1)")
  where reformat (a : b : []) = (head a, head b)

findBAB :: (Char,Char) -> String -> Bool
findBAB (a,b) s
  | a == b    = False
  | otherwise = s =~ [b,a,b]

useSSL (inner,outer) = any checkBABs inner
  where
    checkBABs s = foldl (\t -> \f -> t || f s) False checkBABs'
    checkBABs'  = concat $ map (map findBAB . findABA) outer

main = do
  input <- getContents
  let parsed = parse (parseInput <* eof) "Error" input
  print $ fmap length parsed
  print $ fmap (length . filter useTLS) parsed
  print $ fmap (length . filter useSSL) parsed